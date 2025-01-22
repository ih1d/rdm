module TAL where

import Data.List.NonEmpty (NonEmpty)
import Data.Text.Lazy (Text)

{- TAL block
 - embedded typed assembly block
-}
newtype TalInstr = TalInstr { talBlock :: Asm }
    deriving (Show)

type Asm = (Text, [(Register, TalType)], [Instruction])

data TalType
    = TalInt Int
    | TalLbl Text
    | TalVar Text
    deriving (Show)

data TalVal = RegV Register | WordV Int
    deriving (Show)

data Instruction
    = STR Register Instruction
    | ADD TalVal TalVal
    | IF Register Instruction
    | JMPR Register
    | JMPL Text
    deriving (Show)

data Register
    = R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8
    | R9
    deriving (Show)
