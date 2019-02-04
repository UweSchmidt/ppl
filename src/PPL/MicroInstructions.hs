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

import qualified Data.Array.IArray as IA
import qualified Data.IntMap       as IM

import System.IO

import Control.Exception        ( SomeException
                                , IOException
                                , try
                                )

-- ----------------------------------------

data Instr        = Instr -- dummy
data MValue       = MV    -- dummy
data Address      = LocA Int
                  | AbsA Int

data MStatus      = Ok
                  | AddressViolation Address
                  | PCoutOfRange
                  | IOError String

newtype MProg     = MProg {unMProg :: IA.Array Int Instr}
newtype MSeg      = MSeg  {unMSeg  :: IM.IntMap MValue}
type EvalStack    = [MValue]
type RuntimeStack = [MSeg]

data MState       = MS { instr  :: ! MProg
                       , pc     :: ! Int
                       , stack  :: ! EvalStack
                       , mem    :: ! MSeg
                       , frames :: ! RuntimeStack
                       , status :: ! MStatus
                       }

-- ----------------------------------------

newtype MicroCode a
  = RT { unRT :: ExceptT () (StateT MState IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadState MState
           , MonadError ()
           , MonadIO
           )

runMicroCode :: MicroCode a -> MState -> IO (Either () a, MState)
runMicroCode m st
  = (runStateT . runExceptT . unRT $ m) st

-- ----------------------------------------
--
-- lift IO commands and catch all IO exceptions

io :: IO a -> MicroCode a
io x = do
  r <- liftIO $ try x
  either (abort . IOError . showExc) return $ r
  where
    showExc :: IOException -> String
    showExc = show

abort :: MStatus -> MicroCode a
abort exc = do
  msStatus .= exc
  throwError ()

check :: MStatus -> MicroCode (Maybe a) -> MicroCode a
check exc cmd =
  cmd >>= maybe (abort exc) return

-- ----------------------------------------

msInstr :: Lens' MState MProg
msInstr k ms = (\ new -> ms {instr = new}) <$> k (instr ms)

msPc :: Lens' MState Int
msPc k ms = (\ new -> ms {pc = new}) <$> k (pc ms)

msMem :: Lens' MState MSeg
msMem k ms = (\ new -> ms {mem = new}) <$> k (mem ms)

msStack :: Lens' MState EvalStack
msStack k ms = (\ new -> ms {stack = new}) <$> k (stack ms)

msFrames :: Lens' MState RuntimeStack
msFrames k ms = (\ new -> ms {frames = new}) <$> k (frames ms)

msStatus :: Lens' MState MStatus
msStatus k ms = (\ new -> ms {status = new}) <$> k (status ms)

-- ----------------------------------------

getInstr :: MicroCode Instr
getInstr = check PCoutOfRange getInstr'

getInstr' :: MicroCode (Maybe Instr)
getInstr' = do
  i  <- use msPc
  pg <- uses msInstr unMProg
  let (lb, ub) = IA.bounds pg
  return $
    if (lb <= i && i <= ub)
    then return $ pg IA.! i
    else mzero

getPc :: MicroCode Int
getPc = use msPc

setPc :: Int -> MicroCode ()
setPc = (msPc .=)

incrPc :: MicroCode ()
incrPc = msPc += 1

-- ----------------------------------------

readMem :: Address -> MicroCode MValue
readMem a =
  check (AddressViolation a) $ readMem' a

readMem' :: Address -> MicroCode (Maybe MValue)
readMem' (AbsA addr) = do
  mm <- uses msMem unMSeg
  return $ IM.lookup addr mm

readMem' (LocA addr) = do
  sf <- use msFrames
  return $ do
    f <- sf ^? _head . to unMSeg
    IM.lookup addr f

-- ----------------------------------------
