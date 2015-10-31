module Brain.Type where

import Data.List
import Data.Int
import Data.IORef
import Data.Array.IO
import Control.Applicative

data Command = PInc | PDec | VInc | VDec | POut | PInp | LBgn | LEnd
instance Show Command where
  show PInc = ">"
  show PDec = "<"
  show VInc = "+"
  show VDec = "-"
  show POut = "."
  show PInp = ","
  show LBgn = "["
  show LEnd = "]"

data Exp = Exp Command | Loop [Exp]
instance Show Exp where
  show (Exp t) = show t
  show (Loop es) = "[" ++ concatMap show es ++ "]"

type Pointer = IORef Int
newPointer :: IO Pointer
newPointer = newIORef 0

type Memory = IOArray Int Int
newMemory :: IO Memory
newMemory = newArray (0, 10000) 0

printMem :: Memory -> IO ()
printMem m = getElems m >>= print . map show

data Computer = Computer {
  p :: Pointer
, mem :: Memory
}

data Program = Program [Exp] deriving Show

newComputer :: IO Computer
newComputer = Computer <$> newPointer <*> newMemory
