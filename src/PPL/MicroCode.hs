module PPL.MicroCode where

import           Control.Monad

import           PPL.Error
import           PPL.Instructions
import           PPL.MachineArchitecture
import           PPL.ShowMS
import           PPL.Picture (Picture)

import           System.IO

-- -------------------------------------------------------------------
--
-- machine state transitions

newtype MST a = MST { trans :: MS -> IO (MS, Maybe a) }

instance Functor MST where
    fmap = liftM

instance Applicative MST where
    pure  = return
    (<*>) = ap

instance Monad MST where
    return v
        = MST ( \s -> return (s, Just v))

    MST cmd >>= f
        = MST ( \s ->
                 cmd s >>= \ (s', r) ->
                 case r of
                 Just v  -> trans (f v) s'
                 Nothing -> return (s', Nothing)
               )

-- --------------------
--
-- lift IO comand to MST

io :: IO a -> MST a
io ioa =
    MST ( \s ->
           do
           a <- ioa
           return (s, Just a)
         )

-- --------------------
--
-- lift state modifying function to MST

changeStateWith :: (MS -> MS) -> MST ()
changeStateWith fct
    = MST ( \s ->
            return (fct s, Just ())
          )

-- --------------------
--
-- lift state modifying function to MST

readAndChangeStateWith  :: (MS -> (MS, a)) -> MST a
readAndChangeStateWith fct
    = MST ( \s ->
            let (s', v) = fct s
            in
            return (s', Just v)
          )

-- --------------------
--
-- lift state reading function to MST

readStateWith   :: (MS -> a) -> MST a
readStateWith fct
    = MST ( \s ->
            return (s, Just (fct s))
          )

checkStateWith  :: (MS -> a) -> MST a
checkStateWith  = readStateWith

-- --------------------
--
-- lift compute and IO functions

liftCompute             :: ([a] -> Error a) -> ([a] -> MST a)
liftCompute fct
    = \ vl -> MST ( \ s ->
                    return (case fct vl of
                            (OK v)      -> (s, Just v)
                            (Error err) -> (setExc err s, Nothing)
                           )
                  )

liftSvc                 :: ([a] -> IO (Error a)) -> ([a] -> MST a)
liftSvc fct
    = \ vl -> MST ( \ s ->
                    do
                    res <- fct vl
                    return (case res of
                            (OK v)      -> (s, Just v)
                            (Error err) -> (setExc err s, Nothing)
                           )
                  )

-- --------------------
--
-- state inspection IO command

trc     :: (MS -> String) -> MST ()
trc fct
    = MST ( \s ->
             do
             hPutStr stderr $ fct s
             return (s, Just ())
           )

-- --------------------
--
-- run a MST comand

run :: MS -> MST () -> IO()
run initState (MST cmd)
    = cmd initState >> return ()

-- --------------------
--
-- throw an exception: set status and abort further computation

throw           :: String -> MST a
throw err
    = MST ( \s ->
            return (setExc err s, Nothing)
          )

-- --------------------
--
-- catch all errors

succeed :: MST () -> MST ()
succeed (MST cmd)
    = MST ( \ s ->
            cmd s >>= \ (s', r) ->
            case r of
            Just _v -> return (s', r)
            Nothing -> return (s', Just ())
          )

-- -------------------------------------------------------------------

loadInstr       :: MST Instr
loadInstr
    = do
      pcIsOk <- checkStateWith legalPc
      if pcIsOk
         then readStateWith getInstr
         else do
              illegalPc <- readStateWith showIllegalPC
              throw illegalPc

incrProgCount   :: MST ()
incrProgCount   = changeStateWith incrPc

setProgCount    :: Int -> MST ()
setProgCount p  = changeStateWith (setPc p)

getProgCount    :: MST Int
getProgCount    = readStateWith getPc

allocSF         :: Int -> MST ()
allocSF size    = changeStateWith (allocFrame size)

freeSF          :: MST ()
freeSF
    = do
      empty <- checkStateWith nullFrames
      if not empty
         then changeStateWith freeFrame
         else throw showFrameStackUnderflow

pushMV          :: MV -> MST ()
pushMV v        = changeStateWith (pushValue v)

popMV           :: MST MV
popMV
    = do
      empty <- checkStateWith emptyStack
      if not empty
         then readAndChangeStateWith popValue
         else throw showStackUnderflow

readMV          :: Address -> MST MV
readMV addr
    = do
      addrOk <- checkStateWith (legalMemAddr addr)
      if addrOk
         then readStateWith (readMem addr)
         else do
              illegalAddr <- readStateWith (showIllegalRead addr)
              throw illegalAddr

writeMV         :: Address -> MV -> MST ()
writeMV addr v
    = do
      addrOk <- checkStateWith (legalMemAddr addr)
      if addrOk
         then changeStateWith (writeMem addr v)
         else do
              illegalAddr <- readStateWith (showIllegalWrite addr)
              throw illegalAddr

clearMStatus    :: MST ()
clearMStatus    = changeStateWith clearStatus

-- ----------------------------------------
--
-- push, pop and check ops for raw values

pushInt         :: Int -> MST ()
pushInt         = pushMV . VInt

pushFloat       :: Double -> MST ()
pushFloat       = pushMV . VFloat

pushString      :: String -> MST ()
pushString      = pushMV . VString

pushList        :: [MV] -> MST ()
pushList        = pushMV . VList

pushListPic     :: [Picture] -> MST ()
pushListPic     = pushList . map VPic

pushListString  :: [String] -> MST ()
pushListString  = pushList . map VString

pushPic         :: Picture -> MST ()
pushPic         = pushMV . VPic

pushRA          :: Int -> MST ()
pushRA          = pushMV . VCodeAddr

pushUndef       :: MST ()
pushUndef       = pushMV VUndef

-- --------------------

popVal          :: (MV -> MST a) -> MST a
popVal check    = popMV >>= check

popInt          :: MST Int
popInt          = popVal checkInt

popFloat        :: MST Double
popFloat        = popVal checkFloat

popString       :: MST String
popString       = popVal checkString

popList         :: MST [MV]
popList         = popVal checkList

popList1        :: MST [MV]
popList1        = popList >>= checkNotEmpty

popListPic      :: MST [Picture]
popListPic      = popList1 >>= mapM checkPic

popPic          :: MST Picture
popPic          = popVal checkPic

popRA           :: MST Int
popRA           = popVal checkRA

-- --------------------

checkVal  :: (a -> String) -> (a -> Bool) -> String -> a -> MST a
checkVal show' check err v
  | check v   = return v
  | otherwise = throw $ showIllegalOperand' show' err v

checkNE0 :: (Eq a, Num a, Show a) => a -> MST a
checkNE0 = checkVal show (/= 0) "value /= 0"

checkGT0 :: (Ord a, Num a, Show a) => a -> MST a
checkGT0 = checkVal show (>  0) "value > 0"

checkNotEmpty :: [a] -> MST [a]
checkNotEmpty = checkVal (const "[]") (not . null) "nonempty list"

checkListIx :: ([a], Int) -> MST ([a], Int)
checkListIx (l, ix) = do
  i1 <- checkVal show (>= 0)  "negative list index" ix
  i2 <- checkVal show (< len) ("list index < " ++ show len) i1
  return (l, i2)
  where
    len = length l

checkType :: (MV -> Maybe a) -> String -> MV -> MST a
checkType selComp err v
  = maybe (throw $ showIllegalOperand err v)
          return
          $ selComp v

checkNotUndef   :: MV -> MST MV
checkNotUndef   = checkType selNotUndef  "undefined value found"

checkInt        :: MV -> MST Int
checkInt        = checkType selInt       "int"

checkFloat      :: MV -> MST Double
checkFloat      = checkType selFloat     "float"

checkString     :: MV -> MST String
checkString     = checkType selString    "string"

checkList       :: MV -> MST [MV]
checkList       = checkType selList      "list"

checkPic        :: MV -> MST Picture
checkPic        = checkType selPic       "picture"

checkRA         :: MV -> MST Int
checkRA         = checkType selRA        "return address"

selNotUndef              :: MV -> Maybe MV
selNotUndef VUndef       = Nothing
selNotUndef v            = Just v

selInt                   :: MV -> Maybe Int
selInt (VInt v)          = Just v
selInt _                 = Nothing

selFloat                 :: MV -> Maybe Double
selFloat (VFloat v)      = Just v
selFloat _               = Nothing

selString                :: MV -> Maybe String
selString (VString v)    = Just v
selString _              = Nothing

selList                  :: MV -> Maybe [MV]
selList (VList v)        = Just v
selList _                = Nothing

selPic                   :: MV -> Maybe PPL.Picture.Picture
selPic (VPic v)          = Just v
selPic _                 = Nothing

selRA                    :: MV -> Maybe Int
selRA (VCodeAddr v)      = Just v
selRA _                  = Nothing

-- ----------------------------------------
