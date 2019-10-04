module Compiler where

import qualified Parser.Parser as P
import qualified Parser.Grammar as G
import qualified Numeric as N
import Compiler.OpcodeLoader as Loader

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Char (toLower)
import Data.List (intersperse)
type LabelTable = Map.Map String Int

data Bindings = Bindings {
    labelTable :: LabelTable,
    opCodeTable :: Loader.OpcodeTable,
    aluFunTable :: Loader.AluFunTable
}

buildOpcodeTable :: Loader.OpcodeConfig -> Loader.OpcodeTable
buildOpcodeTable (Loader.Config codes _ ) =
    foldl ins (Map.empty :: Map.Map String String) codes where
        ins m (Loader.Opcode mn op) = Map.insert mn op m

buildAluFunTable :: Loader.OpcodeConfig -> Loader.AluFunTable
buildAluFunTable (Loader.Config _ aluFuns ) =
    foldl ins (Map.empty :: Map.Map String String) aluFuns where
        ins m (Loader.AluFunction nm fn) = Map.insert nm fn m

compile :: String -> String -> IO String
compile sep input = do
    let prog@(G.Lines lns) = P.stringParser input
    opcodes <- Loader.readFromFile
    case opcodes of
        Left err -> return err
        Right config ->
            let 
                labelTab = evalState (buildLabelTable prog) initialState
                opCodeTab = buildOpcodeTable config
                aluFunTab = buildAluFunTable config
                bindings = (Bindings labelTab opCodeTab aluFunTab)
                reducer str (ln, instr) = 
                    str ++ (dumpLine ln instr bindings (concat . (intersperse sep))) ++ "\n"
            in
                return $ foldl reducer "" (zip [0..] lns)

type BuildLabelTableState = (Map.Map String Int, Int)

initialState :: BuildLabelTableState
initialState = (Map.empty :: Map.Map String Int, 0)

buildLabelTable :: G.Program -> State BuildLabelTableState LabelTable
buildLabelTable (G.Lines []) = do
    (table, _ ) <- get
    return table
buildLabelTable (G.Lines (ln:lns)) = do
    case ln of 
        (G.InstructionLine _)-> modify (\(t, l)->(t,l+1))
        (G.LabeledLine (G.Label label) _ ) -> do
            (table, crtLine) <- get
            let table' = if Map.notMember label table then
                    Map.insert label crtLine table
                else
                    table
            put  (table', crtLine+1)
    buildLabelTable (G.Lines lns)


dumpLine :: Int -> G.Line -> Bindings -> ([String] -> String) -> String
dumpLine ln (G.InstructionLine instr) bindings concFun = concFun $ dumpInstruction ln instr bindings
dumpLine ln (G.LabeledLine label instr) bindings concFun = concFun $ dumpInstruction ln instr bindings

dumpInstruction :: Int -> G.Instruction -> Bindings -> [String]
dumpInstruction ln instr (Bindings labelTab opCodeTab aluFunTab) = case instr of 
    (G.InstrI instr) -> dumpInstructionI ln instr opCodeTab labelTab 
    (G.InstrJ instr) -> dumpInstructionJ instr opCodeTab labelTab
    (G.InstrR instr) -> dumpInstructionR instr opCodeTab aluFunTab


dumpInstructionI :: Int -> G.InstructionI -> Loader.OpcodeTable -> LabelTable -> [String]
dumpInstructionI _ (G.InstructionIImm op rt rs imm) opCodeTab _ = 
    [(toOpcode op opCodeTab), (regToBin rs), (regToBin rt), (immToBin 16 imm)]

dumpInstructionI ln (G.InstructionIB1L op rs label) opCodeTab labelTab = 
    [(toOpcode op opCodeTab) , (regToBin rs) , "00000" , (labelDiff 16 ln label labelTab)]

dumpInstructionI _ (G.InstructionIB1Imm op rs imm) opCodeTab _ = 
    [(toOpcode op opCodeTab) , (regToBin rs)  , "00000" , (immToBin 16 imm)]

dumpInstructionI ln (G.InstructionIB2L op rs rt label) opCodeTab labelTab = 
    [(toOpcode op opCodeTab) , (regToBin rs) , (regToBin rt) , (labelDiff 16 ln label labelTab)]

dumpInstructionI _ (G.InstructionIB2Imm op rs rt imm) opCodeTab _ =
    [(toOpcode op opCodeTab) , (regToBin rs) , (regToBin rt) , (immToBin 16 imm)]

dumpInstructionR :: G.InstructionR -> Loader.OpcodeTable -> Loader.AluFunTable -> [String]
dumpInstructionR (G.InstructionRReg op rd rs rt) opCodeTab aluFunTab = 
    [(toOpcode op opCodeTab) , (regToBin rs) , (regToBin rt) , (regToBin rd) , "00000" , (toAluFun op aluFunTab)]
dumpInstructionR (G.InstructionRImm op rd rt shamt) opCodeTab aluFunTab = 
    [(toOpcode op opCodeTab) , "00000" , (regToBin rt) , (regToBin rd) , (immToBin 5 shamt) , (toAluFun op aluFunTab)]

dumpInstructionJ :: G.InstructionJ -> Loader.OpcodeTable -> LabelTable -> [String]
dumpInstructionJ (G.InstructionJ op label) opCodeTab labelTab =
    [(toOpcode op opCodeTab), (lookupLabel 26 label labelTab)]


labelDiff :: Int -> Int -> G.Label -> LabelTable -> String
labelDiff nrBits ln (G.Label label) labelTab = 
    let 
        maybeAddr = Map.lookup label labelTab
    in 
        case maybeAddr of 
            Nothing -> error ("Error: Undefined label: " ++ "\"" ++ label ++ "\"")
            Just addr -> decToTwosComp nrBits (addr - ln - 1)


lookupLabel :: Int -> G.Label -> LabelTable -> String
lookupLabel nrBits (G.Label label) labelTab = 
    let 
        maybeAddr = Map.lookup label labelTab
    in 
        case maybeAddr of 
            Nothing -> error "Error: Undefined label: "  ++ label
            Just addr -> decToTwosComp nrBits addr

padTo :: Char -> Int -> String -> String
padTo c nr str = [c| _ <- [1..l]] ++ str where
    l = (length str) - nr

signExtend :: Int -> String -> String
signExtend bits num = padTo (head num) bits num

zeroExtend :: Int -> String -> String
zeroExtend bits num = padTo '0' bits num

regToBin :: G.Register -> String
regToBin (G.Register r) = (decToTwosComp 5 r)

decToTwosComp :: Int -> Int -> String
decToTwosComp nrBits 0 = ['0'|_<-[1..nrBits]]
decToTwosComp nrBits n =
    let 
        absN = if n < 0 then (-n) else n
        bin = N.showIntAtBase 2 (head . show) absN ""
        padded = ['0'|_ <- [1..(nrBits-(length bin))]] ++ bin
    in
        if n < 0 then
            let 
                invBin = [if d == '0' then '1' else '0' | d <- padded]
                [(num, _)] = N.readInt 2 (\c -> (c == '0') || (c == '1')) (\c -> if c == '0' then 0 else 1) invBin
            in
                N.showIntAtBase 2 (head . show) (num+1) ""
        else
            padded

immToBin :: Int -> G.Immediate -> String
immToBin nrBits (G.Immediate imm) = decToTwosComp nrBits imm