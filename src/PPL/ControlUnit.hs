module PPL.ControlUnit
    (execProg)
where

import           Control.Monad           (void, when)

import           PPL.Instructions        (Dest (Disp), Instr (..))

import           PPL.MachineArchitecture (MS, statusIsOk, statusIsTerminated)

import           PPL.MicroCode           (MST (MST), allocSF, checkNotUndef,
                                          checkStateWith, freeSF, getProgCount,
                                          incrProgCount, loadInstr, popInt,
                                          popMV, popRA, pushFloat, pushInt,
                                          pushList, pushMV, pushRA, pushString,
                                          pushUndef, readMV, setProgCount,
                                          succeed, throw, trc, writeMV)

import           PPL.OPCode              (exOP, exSVC)

import           PPL.ShowMS              (showCurInstr, showMS)

-- -------------------------------------------------------------------
-- exec program

execProg        :: MS -> IO ()
execProg
    = runProg startProg

runProg :: MST () -> MS -> IO ()
runProg (MST fct) initState
    = void $ fct initState

startProg       :: MST ()
startProg
    = do
      trc (const "start execution\n")
      continueProg

continueProg    :: MST ()
continueProg
    = do
      trc showCurInstr
      succeed execInstr
      checkMStatus

execInstr       :: MST ()
execInstr
    = do
      instr' <- loadInstr
      incrProgCount
      intpInstr instr'

checkMStatus    :: MST ()
checkMStatus
    = do
      cont <- checkStateWith statusIsOk
      if cont
         then continueProg
         else do
              term <- checkStateWith statusIsTerminated
              if term
                 then termProg
                 else trc showMS

termProg                :: MST ()
termProg
    = trc (const "\nexecution terminated\n")

-- -------------------------------------------------------------------

intpInstr       :: Instr -> MST ()

intpInstr (LoadI i)     = pushInt    i
intpInstr (LoadF f)     = pushFloat  f
intpInstr (LoadS s)     = pushString s
intpInstr  LoadU        = pushUndef
intpInstr  LoadEL       = pushList   []

intpInstr (Load addr)
  = readMV addr >>= checkNotUndef >>= pushMV

intpInstr (Compute op)
    = exOP op

intpInstr (Store addr)
    = popMV >>= writeMV addr

intpInstr  Pop
    = do
      _v <- popMV
      return ()

intpInstr  Dup
    = do
      v <- popMV
      pushMV v
      pushMV v

intpInstr (PushJ (Disp d))
    = do
      pc' <- getProgCount
      pushRA pc'
      setProgCount (pc' - 1 + d)

intpInstr  PopJ
    = popRA >>= setProgCount

intpInstr (Jump (Disp d))
    = do
      pc' <- getProgCount
      setProgCount (pc' - 1 + d)

intpInstr (Branch cond (Disp d))
    = do
      b <- popInt
      when ((b /= 0) == cond) $
        do
          pc' <- getProgCount
          setProgCount (pc' - 1 + d)

intpInstr (Entry len)
    = allocSF len

intpInstr Exit
    = freeSF

intpInstr (SysCall call)
    = exSVC call

intpInstr instr'
    = throw ("unimplemented: " ++ show instr')

-- -------------------------------------------------------------------
