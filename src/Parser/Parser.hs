module Parser.Parser(stringParser, fileParser) where

import System.IO
import Control.Monad
import qualified Text.Parsec as P
import Text.Parsec ((<|>), (<?>))
import Text.Parsec.String (Parser(..))
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Char as P
import Data.Char
import qualified Numeric as N

import qualified Parser.Grammar as G
import qualified Parser.Lexer as L

program :: Parser G.Program
program = programLines

programLines :: Parser G.Program
programLines = P.sepEndBy programLine (L.whitespaceOrComment) >>= \lns -> return (G.Lines lns)

programLine :: Parser G.Line
programLine =
    L.spacesOrTabs >> (instr <|> labelLine) where
        instr = P.try (instruction >>= \l -> return (G.InstructionLine l))

labelLine :: Parser G.Line
labelLine = do
    l <- L.label
    L.spacesOrTabs
    L.colon
    i <- instruction
    return (G.LabeledLine (G.Label l) i)

instruction :: Parser G.Instruction
instruction = P.try ((P.choice (map (P.try) instrIs) >>= \i -> return (G.InstrI i)) <|>
              ((P.try instructionRReg <|> instructionROpImm) >>= \i -> return (G.InstrR i)) <|>
              (P.try instructionJ >>= \i -> return (G.InstrJ i))) where
                instrIs = [instructionIImm, instructionIB2L, instructionIB2Imm, instructionIB1L, instructionIB1Imm]

instructionIImm :: Parser G.InstructionI
instructionIImm = do
    mnemonic <- P.choice $ map (L.keyword) ["addi", "addiu", "slti", "sltiu", "andi", "ori", "xori", "lw", "sw"]
    L.spacesOrTabs
    rtStr <- L.registerName
    L.comma
    rsStr <- L.registerName
    L.comma
    imm <- immediate
    let rt = G.Register (read rtStr :: Int)
        rs = G.Register (read rsStr :: Int)
        op = read (capitalized mnemonic) :: G.InstructionIOpImm
    return (G.InstructionIImm op rt rs imm)

instructionIB2L :: Parser G.InstructionI
instructionIB2L = do
    mnemonic <- P.choice $ map (L.keyword) ["beq", "bne"]
    L.spacesOrTabs
    rtStr <- L.registerName
    L.comma
    rsStr <- L.registerName
    L.comma
    labelStr <- L.label
    let rt = G.Register (read rtStr :: Int)
        rs = G.Register (read rsStr :: Int)
        imm = G.Label labelStr
        op = read (capitalized mnemonic) :: G.InstructionIOpB2
    return (G.InstructionIB2L op rt rs imm)

instructionIB2Imm :: Parser G.InstructionI
instructionIB2Imm = do
    mnemonic <- P.choice $ map (L.keyword) ["beq", "bne"]
    L.spacesOrTabs
    rtStr <- L.registerName
    L.comma
    rsStr <- L.registerName
    L.comma
    imm <- immediate
    let rt = G.Register (read rtStr :: Int)
        rs = G.Register (read rsStr :: Int)
        op = read (capitalized mnemonic) :: G.InstructionIOpB2
    return (G.InstructionIB2Imm op rt rs imm)

instructionIB1Imm :: Parser G.InstructionI
instructionIB1Imm = do
    mnemonic <- P.choice $ map (L.keyword) ["blez", "bgtz", "bltz", "bgez"]
    L.spacesOrTabs
    rtStr <- L.registerName
    L.comma
    imm <- immediate
    let rt = G.Register (read rtStr :: Int)
        op = read (capitalized mnemonic) :: G.InstructionIOpB1
    return (G.InstructionIB1Imm op rt imm)

instructionIB1L :: Parser G.InstructionI
instructionIB1L = do
    mnemonic <- P.choice $ map (L.keyword) ["blez", "bgtz", "bltz", "bgez"]
    L.spacesOrTabs
    rtStr <- L.registerName
    L.comma
    labelStr <- L.label
    let rt = G.Register (read rtStr :: Int)
        imm = G.Label labelStr
        op = read (capitalized mnemonic) :: G.InstructionIOpB1
    return (G.InstructionIB1L op rt imm)

instructionRReg :: Parser G.InstructionR
instructionRReg = do
    mnemonic <- P.choice $ map (L.keyword) ["add", "addu", "sub", "subu", "and", "or","xor", "nor", "slt", "sltu", "sllv", "srlv", "sarv"]
    L.spacesOrTabs
    rdStr <- L.registerName
    L.comma
    rsStr <- L.registerName
    L.comma
    rtStr <- L.registerName
    let rt = G.Register (read rtStr :: Int)
        rs = G.Register (read rsStr :: Int)
        rd = G.Register (read rdStr :: Int)
        op = read (capitalized mnemonic) :: G.InstructionROpReg
    return (G.InstructionRReg op rd rs rt)

instructionROpImm :: Parser G.InstructionR
instructionROpImm = do
    mnemonic <- P.choice $ map (L.keyword) ["sll", "srl", "sra"]
    L.spacesOrTabs
    rdStr <- L.registerName
    L.comma
    rtStr <- L.registerName
    L.comma
    imm <- immediate
    let rt = G.Register (read rtStr :: Int)
        rd = G.Register (read rdStr :: Int)
        op = read (capitalized mnemonic) :: G.InstructionROpImm
    return (G.InstructionRImm op rd rt imm)

instructionJ :: Parser G.InstructionJ
instructionJ = do
    mnemonic <- P.choice $ map (L.keyword) ["j","jal"]
    L.spacesOrTabs
    labelStr <- L.label
    let imm = G.Label labelStr
        op = read (capitalized mnemonic) :: G.InstructionJOp
    return (G.InstructionJ op imm)

immediate :: Parser G.Immediate
immediate = (P.try $ do
    P.char '-'
    n <- imm
    return (G.Immediate (-n)))
    <|> (P.try $ do
    n <- imm
    return (G.Immediate n)) where
        imm = (P.try $ L.decImmediate) <|> (P.try $ L.hexImmediate) <|> ( P.try $ L.binImmediate)

capitalized :: String -> String
capitalized (x:xs) = (toUpper x):xs

stringParser :: String -> G.Program
stringParser str =
    case P.parse program "" str of
        Left e -> error (show e)
        Right res -> res

fileParser :: String -> IO G.Program
fileParser fileName = do
    result <- P.parseFromFile program fileName
    case result of
        Left err  -> error (show err)
        Right program  -> return program