module Brain.Parser where

import Control.Applicative ((<*), (*>), (<$>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Brain.Type

brainfuck :: Parser Program
brainfuck = Program <$> many field

field :: Parser Exp
field = try comm <|> try loop

loop :: Parser Exp
loop = lbgn *> (Loop <$> many field) <* lend

comm :: Parser Exp
comm = Exp <$> (pinc <|> pdec <|> vinc <|> vdec <|> pout <|> pinp)

pinc, pdec, vinc, vdec, pout, pinp :: Parser Command
pinc = char '>' >> return PInc
pdec = char '<' >> return PDec
vinc = char '+' >> return VInc
vdec = char '-' >> return VDec
pout = char '.' >> return POut
pinp = char ',' >> return PInp

lbgn, lend :: Parser Ctrl
lbgn = char '[' >> return LBgn
lend = char ']' >> return LEnd
