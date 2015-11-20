module Brain.Eval where

import Data.Char
import Data.IORef
import Data.Array.IO
import Data.Foldable
import Control.Monad

import Brain.Type
import Brain.Machine

eval :: (Num a, Enum a, Integral a, Read a) => Machine a -> Program -> IO ()
eval m (Program prog) = do
  evalMap m prog
  putStrLn ""

evalExp :: (Num a, Enum a, Integral a, Read a) => Machine a -> Exp -> IO (Machine a)
evalExp m@(Machine b v a) exp =
  case exp of
    Exp PInc -> return . incPoint $ m
    Exp PDec -> return . decPoint $ m
    Exp VInc -> return . incValue $ m
    Exp VDec -> return . decValue $ m
    Exp POut -> (putChar . chr . fromIntegral $ v) >> return m
    Exp PInp -> do
      inp <- readLn
      return $ Machine b inp a
    l@(Loop es) ->
      if v == 0
      then return m
      else evalMap m es >>= flip evalExp l

evalMap :: (Num a, Enum a, Integral a, Read a) => Machine a -> [Exp] -> IO (Machine a)
evalMap m es = foldlM evalExp m es
