module Brain.Eval where

import Data.Char
import Data.IORef
import Data.Array.IO
import Control.Monad

import Brain.Type

eval :: Machine -> Program -> IO ()
eval c (Program es) = do
  evalMap c es
  putStrLn ""

evalExp :: Machine -> Exp -> IO ()
evalExp (Machine p m) (Exp PInc) = modifyIORef p succ
evalExp (Machine p m) (Exp PDec) = modifyIORef p pred
evalExp (Machine p m) (Exp VInc) = readIORef p >>= modifyArray succ m
evalExp (Machine p m) (Exp VDec) = readIORef p >>= modifyArray pred m
evalExp (Machine p m) (Exp POut) = readIORef p >>= readArray m >>= putChar . chr
evalExp (Machine p m) (Exp PInp) = do
  pointer <- readIORef p
  input <- (readLn :: IO Int)
  writeArray m pointer input
evalExp c@(Machine p m) l@(Loop es) = do
  pointer <- readIORef p
  v <- readArray m pointer
  if v == 0
  then return ()
  else do { evalMap c es; evalExp c l }

evalMap :: Machine -> [Exp] -> IO ()
evalMap c es = mapM_ (evalExp c) es

modifyArray :: (MArray a e m, Ix i) => (e -> e) -> a i e -> i -> m ()
modifyArray f arr i = do
  v <- readArray arr i
  writeArray arr i (f v)
