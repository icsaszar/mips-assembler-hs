module Parser.Grammar where

data Program =  Lines [Line]
                deriving (Show)


data Line = InstructionLine Instruction
          | LabeledLine Label Instruction
          deriving (Show)

data Instruction    = InstrI InstructionI
                    | InstrJ InstructionJ
                    | InstrR InstructionR
                    deriving (Show)

data InstructionI   = InstructionIImm InstructionIOpImm Register Register Immediate
                    | InstructionIB1L InstructionIOpB1 Register Label
                    | InstructionIB1Imm InstructionIOpB1 Register Immediate
                    | InstructionIB2L InstructionIOpB2 Register Register Label
                    | InstructionIB2Imm InstructionIOpB2 Register Register Immediate
                    deriving (Show)

data InstructionR   = InstructionRReg InstructionROpReg Register Register Register
                    | InstructionRImm InstructionROpImm Register Register Immediate
                    deriving (Show)

data InstructionJ   = InstructionJ InstructionJOp Label
                    deriving (Show)

newtype Register = Register Int deriving (Show)
newtype Immediate = Immediate Int deriving (Show)
newtype Label = Label String deriving (Show)

data InstructionIOpImm  = Addi
                        | Addiu
                        | Subi
                        | Slti
                        | Sltiu
                        | Andi
                        | Ori
                        | Xori
                        | Lw
                        | Sw
                        deriving (Show, Read)

data InstructionIOpB2   = Beq
                        | Bne
                        deriving (Show, Read)

data InstructionIOpB1   = Blez
                        | Bgtz
                        | Bltz
                        | Bgez
                        deriving (Show, Read)



data InstructionJOp = J
                    | Jal
                    deriving (Show, Read)

data InstructionROpReg  = Add
                        | Addu
                        | Sub
                        | Subu
                        | And
                        | Or
                        | Xor
                        | Nor
                        | Slt
                        | Sltu
                        | Sllv
                        | Srlv
                        | Sarv
                        deriving (Show, Read)

data InstructionROpImm  = Sll
                        | Srl
                        | Sra
                        deriving (Show, Read)
