module Brain.Type where

import Data.List
import Data.Int
import Data.IORef
import Data.Array.IO
import Control.Applicative

data Command = PInc | PDec | VInc | VDec | POut | PInp
instance Show Command where
  show PInc = ">"
  show PDec = "<"
  show VInc = "+"
  show VDec = "-"
  show POut = "."
  show PInp = ","

data Ctrl = LBgn | LEnd
instance Show Ctrl where
  show LBgn = "["
  show LEnd = "]"

data Exp = Exp Command | Loop [Exp]
instance Show Exp where
  show (Exp t) = show t
  show (Loop es) = "[" ++ concatMap show es ++ "]"

newtype Program = Program [Exp] deriving Show
