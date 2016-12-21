{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PPL.MicroInstructions where

import Control.Applicative (Applicative(..))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad

import Control.Lens

import Data.Array.IArray (bounds, (!))

import           PPL.Instructions
import           PPL.MachineArchitecture
import           PPL.ShowMS

import System.IO

import Control.Exception        ( SomeException
                                , IOException
                                , try
                                )

-- ----------------------------------------

newtype MicroCode a
  = RT { unRT :: ExceptT MStatus (StateT MState IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadState MState
           , MonadError MStatus
           , MonadIO
           )

runMicroCode :: MicroCode a -> MS -> IO (Either MStatus a, MState)
runMicroCode m st
  = (runStateT . runExceptT . unRT $ m) st

-- ----------------------------------------
--
-- lift IO commands and catch all IO exceptions

io :: IO a -> MicroCode a
io x = do
  r <- liftIO $ try x
  either (throwError . Exc . showExc) return $ r
  where
    showExc :: IOException -> String
    showExc = show

-- ----------------------------------------

msInstr :: Lens' MState MProg
msInstr k ms = (\ new -> ms {instr = new}) <$> k (instr ms)

msPc :: Lens' MState Int
msPc k ms = (\ new -> ms {pc = new}) <$> k (pc ms)

msMem :: Lens' MState Mem
msMem k ms = (\ new -> ms {mem = new}) <$> k (mem ms)

msStack :: Lens' MState Stack
msStack k ms = (\ new -> ms {stack = new}) <$> k (stack ms)

msFrames :: Lens' MState [Mem]
msFrames k ms = (\ new -> ms {frames = new}) <$> k (frames ms)

msStatus :: Lens' MState MStatus
msStatus k ms = (\ new -> ms {status = new}) <$> k (status ms)

-- ----------------------------------------

getInstr :: MicroCode (Maybe Instr)
getInstr = do
  i  <- use msPc
  pg <- use msInstr
  let (lb, ub) = bounds pg
  return $
    if (lb <= i && i <= ub)
    then return $ pg ! i
    else mzero

getPc :: MicroCode Int
getPc = use msPc

setPc :: Int -> MicroCode ()
setPc = (msPc .=)

incrPc :: MicroCode ()
incrPc = msPc += 1

-- ----------------------------------------

readMem :: Address -> MicroCode (Maybe MV)
readMem (AbsA addr) = do
  mm <- use msMem
  return $
    if 0 <= addr && addr < length mm
    then return $ mm !! addr
    else mzero

readMem (LocA addr) = do
  sf <- use msFrames
  return $ do
    f <- sf ^? _head
    if 0 <= addr && addr < length f
      then return $ f !! addr
      else mzero

-- ----------------------------------------
