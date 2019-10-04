module Main where


import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as HU


import qualified Compiler as C
import qualified Parser.Grammar as G
import qualified Parser.Lexer as L
import qualified Compiler.OpcodeLoader as Loader
import qualified Data.Map.Strict as Map
import qualified Text.Parsec as P

main :: IO ()
main = T.defaultMain tests


tests = 
    T.testGroup "Unit Tests"
    [
        T.testGroup "Code generation tests" 
        [
            instructionJTests
            , instructionITests
            , instructionRTests
        ],
        T.testGroup "Utility function tests" [
            decToTwosCompTests
        ],
        T.testGroup "Immediate value parsing tests" 
        [
            parseBinaryTests,
            parseHexTests
        ]
    ]

instructionJTests = T.testGroup "Instructiuon J tests" 
                    [
                        testDumpInstructionJ
                    ]

testDumpInstructionJ = 
    HU.testCase "The generated code for j TEST-LABEL is [\"000010\", \"00000000000000000000000010\"]" $
    HU.assertEqual [] expected actual where
        actual = C.dumpInstructionJ instr codes labels
        expected = ["000010", "00000000000000000000000010"]
        instr = G.InstructionJ G.J (G.Label "TEST-LABEL")
        codes = Map.singleton "j" "000010"
        labels = Map.singleton "TEST-LABEL" 2


instructionITests = T.testGroup "Instruction I tests"
                    [
                        testDumpInstructionIImm
                        , testDumpInstructionIB2Imm
                    ]


testDumpInstructionIImm = 
    HU.testCase ("The generated code for addi $16, $17, 5 is " ++ show expected) $
    HU.assertEqual [] expected actual where
        actual = C.dumpInstructionI 0 instr codes labels
        expected = ["001000", "10001", "10000", "0000000000000101"]
        instr = (G.InstructionIImm G.Addi rt rs (G.Immediate 5))
        rs = G.Register 17
        rt = G.Register 16
        codes = Map.singleton "addi" "001000"
        labels = Map.empty :: Map.Map String Int

testDumpInstructionIB2Imm = 
    HU.testCase ("The generated code for beq $7, $1, 32 is " ++ show expected) $
    HU.assertEqual [] expected actual where
        actual = C.dumpInstructionI 0 instr codes labels
        expected = ["000100", "00111", "00001", "0000000000100000"]
        instr = (G.InstructionIB2Imm G.Beq rs rt (G.Immediate 32))
        rs = G.Register 7
        rt = G.Register 1
        codes = Map.singleton "beq" "000100"
        labels = Map.empty :: Map.Map String Int


instructionRTests = T.testGroup "Instruction R tests"
                    [
                        testDumpInstructionRReg
                    ]

testDumpInstructionRReg =
    HU.testCase ("The generated code for add $16 $17, $18 is " ++ show expected) $
    HU.assertEqual [] expected actual where
        actual = C.dumpInstructionR instr codes funs
        expected = ["000000", "10001", "10010", "10000", "00000", "100000"]
        instr = (G.InstructionRReg G.Add rd rs rt)
        rs = G.Register 17
        rt = G.Register 18
        rd = G.Register 16
        codes = Map.singleton "add" "000000"
        funs = Map.singleton "add" "100000"


testDumpInstructionRImm =
    HU.testCase ("The generated code for sll $16, $17, $18 is " ++ show expected) $
    HU.assertEqual [] expected actual where
        actual = C.dumpInstructionR instr codes funs
        expected = ["000000", "10001", "10010", "10000", "00000", "100000"]
        instr = (G.InstructionRReg G.Add rs rt rd)
        rs = G.Register 17
        rt = G.Register 18
        rd = G.Register 16
        codes = Map.singleton "add" "000000"
        funs = Map.singleton "add" "100000"

decToTwosCompTests = T.testGroup "Decimal to two's complement tests" [
    testDecToTwosComp num expected | (num, expected) <- zip [1, 0, 8, -1, -2, -8] ["00001", "00000", "01000", "11111", "11110", "11000"]
    ]

testDecToTwosComp :: Int -> String -> T.TestTree
testDecToTwosComp num expected =
    HU.testCase ("The two's complement of " ++ show num ++ " is " ++ expected) $
    HU.assertEqual [] expected actual where
        actual = C.decToTwosComp 5 num


parseBinaryTests = T.testGroup "Binary immediate parsing tests" [
    testParseBinary num expected | (num, expected) <- zip ["0b1", "0b11", "0b111", "0b1000"] [1, 3, 7, 8]
    ]

testParseBinary :: String -> Int -> T.TestTree
testParseBinary str expected = 
    HU.testCase (str ++ " is parsed as " ++ show expected) $
    case P.parse L.binImmediate "" str of
        Left err -> HU.assertFailure $ "Parse failed with: " ++ show err
        Right actual -> HU.assertEqual [] expected actual where

parseHexTests = T.testGroup "Hexadecimal immediate parsing tests" [
    testParseHex num expected | (num, expected) <- zip ["0x1", "0xA", "0xFF", "0x100"] [1, 10, 255, 256]
    ]

testParseHex :: String -> Int -> T.TestTree
testParseHex str expected =
    HU.testCase (str ++ " is parsed as " ++ show expected) $
    case P.parse L.hexImmediate "" str of
        Left err -> HU.assertFailure $ "Parse failed with: " ++ show err
        Right actual -> HU.assertEqual [] expected actual where