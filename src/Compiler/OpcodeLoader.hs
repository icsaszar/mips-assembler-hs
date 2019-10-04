{-# LANGUAGE OverloadedStrings #-}
module Compiler.OpcodeLoader where

import qualified Parser.Grammar as G
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Aeson
import Data.Char
import qualified Data.ByteString.Lazy as B

type OpcodeTable = Map.Map String String

errNotFound op = error "Error instruction " ++ show op ++ " not found in table"

class (Show a) => Op a where
    toOpcode :: a -> OpcodeTable -> String
    toOpcode op table = case Map.lookup (map toLower $ show op) table of 
        Just code -> code
        Nothing -> errNotFound op


instance Op G.InstructionIOpImm

instance Op G.InstructionIOpB2

instance Op G.InstructionIOpB1

instance Op G.InstructionJOp

instance Op G.InstructionROpReg

instance Op G.InstructionROpImm

type AluFunTable = Map.Map String String

class (Show a) => AluFun a where
    toAluFun :: a -> AluFunTable -> String
    toAluFun op table = case Map.lookup (map toLower $ show op) table of 
        Just code -> code
        Nothing -> error "Error alu function not found in table"

instance AluFun G.InstructionROpReg where

instance AluFun G.InstructionROpImm where

data Opcode = Opcode {
    mneomic ::  String,
    opcode :: String
} deriving (Show)
 
data AluFunction = AluFunction {
    name :: String,
    aluFunc :: String
} deriving (Show)

data OpcodeConfig = Config {
    opcodes :: [Opcode],
    aluFunctions :: [AluFunction]
} deriving(Show)

instance FromJSON Opcode where
    parseJSON (Object o) = 
        Opcode <$> o .: "mneomic"
               <*> o .: "code"
    parseJSON _ = mzero

instance FromJSON AluFunction where
    parseJSON (Object o) = 
        AluFunction <$> o .: "name"
                    <*> o .: "code"
    parseJSON _ = mzero

instance FromJSON OpcodeConfig where
    parseJSON (Object o) = 
        Config <$> o .: "opcodes"
               <*> o .: "alu_functions"
    parseJSON _ = mzero



readJSONFile :: IO B.ByteString
readJSONFile = do
    let fileName = "config/codes.json"
    B.readFile fileName

readFromFile :: IO (Either String OpcodeConfig)
readFromFile = (eitherDecode <$> readJSONFile) :: IO (Either String OpcodeConfig)