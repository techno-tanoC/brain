module Brain.Eval where

import Data.Char
import Data.IORef
import Data.Array.IO
import Control.Monad

import Brain.Type

eval :: Computer -> Program -> IO ()
eval c (Program es) = do
  putStrLn ""
  evalMap c es
  putStrLn ""

evalExp :: Computer -> Exp -> IO ()
evalExp (Computer p m) (Exp PInc) = modifyIORef p succ
evalExp (Computer p m) (Exp PDec) = modifyIORef p pred
evalExp (Computer p m) (Exp VInc) = readIORef p >>= modifyArray succ m
evalExp (Computer p m) (Exp VDec) = readIORef p >>= modifyArray pred m
evalExp (Computer p m) (Exp POut) = readIORef p >>= readArray m >>= putChar . chr
evalExp (Computer p m) (Exp PInp) = do
  pointer <- readIORef p
  input <- (readLn :: IO Int)
  writeArray m pointer input
evalExp c@(Computer p m) l@(Loop es) = do
  pointer <- readIORef p
  v <- readArray m pointer
  if v == 0
  then return ()
  else do { evalMap c es; evalExp c l }

evalMap :: Computer -> [Exp] -> IO ()
evalMap c es = mapM_ (evalExp c) es

modifyArray :: (MArray a e m, Ix i) => (e -> e) -> a i e -> i -> m ()
modifyArray f arr i = do
  v <- readArray arr i
  writeArray arr i (f v)
