module Brain.Gochiusa
  ( gochiusa
  , convertGochiusa
  ) where

import Control.Applicative ((<*), (*>), (<$>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Brain.Type

gochiusa :: Parser Program
gochiusa = Program <$> many field

field :: Parser Exp
field = try comm <|> try loop

loop :: Parser Exp
loop = lbgn *> (Loop <$> many field) <* lend

comm :: Parser Exp
comm = Exp <$> (pinc <|> pdec <|> vinc <|> vdec <|> pout <|> pinp)

pinc, pdec, vinc, vdec, pout, pinp :: Parser Command
pinc = string "ココア" >> return PInc
pdec = string "チノ" >> return PDec
vinc = string "リゼ" >> return VInc
vdec = string "千夜" >> return VDec
pout = string "シャロ" >> return POut
pinp = string "モカ" >> return PInp

lbgn, lend :: Parser Ctrl
lbgn = string "メグ" >> return LBgn
lend = string "マヤ" >> return LEnd

convertGochiusa :: Program -> String
convertGochiusa (Program prog) = prog >>= conv
  where
    conv (Exp c) = case c of
      PInc -> "ココア"
      PDec -> "チノ"
      VInc -> "リゼ"
      VDec -> "千夜"
      POut -> "シャロ"
      PInp -> "モカ"
    conv (Loop cs) = "メグ" ++ (cs >>= conv) ++ "マヤ"
