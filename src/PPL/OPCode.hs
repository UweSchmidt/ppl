module PPL.OPCode
    ( exOP
    , exSVC
    ) where

import           System.Environment      (getArgs)

import           PPL.Instructions        (Opcode (..), Subroutine)
import           PPL.MachineArchitecture (programAborted, programTerminated)
import           PPL.MicroCode           (MST, checkGT0, checkListIx, checkNE0,
                                          io, popFloat, popInt, popList,
                                          popList1, popListPic, popMV, popPic,
                                          popString, pushFloat, pushInt,
                                          pushList, pushListPic, pushListString,
                                          pushMV, pushPic, pushString,
                                          pushUndef, throw, trc)
import           PPL.Picture             (Picture, aboveMx, bitmapPic,
                                          blackAndWhitePic, concatHMx,
                                          concatVMx, cutMx, diffPic, flipDMx,
                                          flipHMx, flipVMx, gammaPic, greyPic,
                                          heightMx, invDiffPic, invMeanPic,
                                          invertPic, maxPic, meanPic, mergeHMx,
                                          mergeVMx, minPic, mulPic, partHMx,
                                          partVMx, pasteMx, readPictureFile,
                                          reduceColorPic, replicateMx,
                                          resizePic, rotateMx, scaleMx, shiftMx,
                                          shrinkMx, sideBySideMx, splitHMx,
                                          splitVMx, widthMx, writePictureFile)
import           PPL.ShowMS              (showMS)

-- ----------------------------------------
--
-- execute compute op

exOP :: Opcode -> MST ()

-- exec binary int ops
exOP OPaddi  = exInt2 (+)
exOP OPsubi  = exInt2 (-)
exOP OPmuli  = exInt2 (*)
exOP OPdivi  = exOP2 popInt (popInt >>= checkNE0) pushInt div
exOP OPmodi  = exOP2 popInt (popInt >>= checkNE0) pushInt mod
exOP OPmaxi  = exInt2 max
exOP OPmini  = exInt2 min
exOP OPeqi   = exInt2 (\ x y -> fromEnum (x == y))
exOP OPgei   = exInt2 (\ x y -> fromEnum (x >= y))
exOP OPgti   = exInt2 (\ x y -> fromEnum (x >  y))

-- exec unary int ops
exOP OPincri = exInt1 (+ 1)
exOP OPdecri = exInt1 (\ x -> x - 1)

-- exec binary float ops
exOP OPaddf  = exFloat2 (+)
exOP OPsubf  = exFloat2 (-)
exOP OPmulf  = exFloat2 (*)
exOP OPdivf  = exFloat2 (/)
exOP OPmaxf  = exFloat2 max
exOP OPminf  = exFloat2 min
exOP OPeqf   = exFloat2Int (\ x y -> fromEnum (x == y))
exOP OPgef   = exFloat2Int (\ x y -> fromEnum (x >= y))
exOP OPgtf   = exFloat2Int (\ x y -> fromEnum (x >  y))

-- conversions
exOP OPi2s   = exOP1 popInt   pushString show
exOP OPf2s   = exOP1 popFloat pushString show
exOP OPi2f   = exOP1 popInt   pushFloat  (fromInteger . toInteger)
exOP OPtrunc = exOP1 popFloat pushInt    truncate
exOP OPround = exOP1 popFloat pushInt    round

-- string ops
exOP OPconcs = exOP2 popString popString pushString (++)

-- list ops
exOP OPisemptyl = exOP1  popList pushInt  (fromEnum . null)
exOP OPlengthl  = exOP1  popList pushInt  length
exOP OPtaill    = exOP1  popList1 pushList tail
exOP OPconcl    = exOP2  popList popList pushList (++)
exOP OPconsl    = exOP2  popList popMV    pushList (flip (:))
exOP OPappendl  = exOP2  popList popMV    pushList
                         (\ xs x -> xs ++ [x])

-- here the argument check is more complicated than
-- with the other checks,
-- the relation between the 2 arguments must be checked
exOP OPindexl   = exOP1  ( ( do i <- popInt
                                l <- popList
                                return (l, i)
                           )
                           >>= checkListIx
                         ) pushMV (uncurry (!!))

exOP OPwidth    = exOP1  popPic pushInt widthMx
exOP OPheight   = exOP1  popPic pushInt heightMx
exOP OPblack    = exOP2  popInt popInt pushPic (greyPic 0.0)
exOP OPwhite    = exOP2  popInt popInt pushPic (greyPic 1.0)
exOP OPgrey     = exOP3  popFloat popInt popInt pushPic greyPic
exOP OPgamma    = exOP2  popPic popFloat pushPic (flip gammaPic)
exOP OPinvert   = exPic1 invertPic
exOP OPbitmap   = exPic1 bitmapPic
exOP OPblackAndWhite
                = exPic1 blackAndWhitePic
exOP OPreduceColor
                = exOP2  popPic popInt pushPic
                         (flip reduceColorPic)
exOP OPflipHorizontal
                = exPic1 flipHMx
exOP OPflipVertical
                = exPic1 flipVMx
exOP OPflipDiagonal
                = exPic1 flipDMx
exOP OProtate   = exPic1 rotateMx
exOP OPshift    = exPicInt2 shiftMx
exOP OPcut      = exOP5 popPic popInt popInt popInt popInt
                        pushPic
                        (\ p x y w h -> cutMx x y w h p)
exOP OPpaste    = exOP4 popPic popPic popInt popInt pushPic
                        (\ p1 p2 x y -> pasteMx x y p2 p1)
exOP OPscale    = exPicInt2 scaleMx
exOP OPshrink   = exPicInt2 shrinkMx
exOP OPreplicate= exPicInt2 replicateMx
exOP OPresize   = exPicInt2 resizePic
exOP OPsideBySide
                = exPic2 sideBySideMx
exOP OPabove    = exPic2 aboveMx
exOP OPpartitionHorizontal
                = exPicPart partHMx
exOP OPpartitionVertical
                = exPicPart partVMx
exOP OPsplitHorizontal
                = exPicPart splitHMx
exOP OPsplitVertical
                = exPicPart splitVMx
exOP OPmergeHorizontal
                = exPic2 mergeHMx
exOP OPmergeVertical
                = exPic2 mergeVMx
exOP OPconcatHorizontal
                = exOP1 popListPic pushPic concatHMx
exOP OPconcatVertical
                = exOP1 popListPic pushPic concatVMx
exOP OPmean     = exPic2 meanPic
exOP OPdiff     = exPic2 diffPic
exOP OPinverseMean
                = exPic2 invMeanPic
exOP OPinverseDiff
                = exPic2 invDiffPic
exOP OPmulp     = exPic2 mulPic
exOP OPmaxp     = exPic2 maxPic
exOP OPminp     = exPic2 minPic

exOP OPterminate
                = throw programTerminated
exOP OPabort    = do s <- popString
                     throw $ programAborted ++ ": " ++ s

{- redundant, pattern match complete
exOP op         = throw $ "unimplemented op: " ++ showOpCode op
-}

-- --------------------
-- unary and binary ops

exOP1 :: MST a
      -> (r -> MST ())
      -> (a -> r) -> MST ()
exOP1 pop push op = (op <$> pop) >>= push

-- --------------------

exOP2 :: MST a -> MST b
      -> (r -> MST ())
      -> (a -> b -> r) -> MST ()
exOP2 pop1 pop2 push op
  = (flip op <$> pop2 <*> pop1) >>= push

-- --------------------

exOP3 :: MST a -> MST b -> MST c
      -> (r -> MST ())
      -> (a -> b -> c -> r) -> MST ()
exOP3 pop1 pop2 pop3 push op
  = (op' <$> pop3 <*> pop2 <*> pop1) >>= push
    where
      op' z y x = op x y z

-- --------------------

exOP4 :: MST a -> MST b -> MST c -> MST d
      -> (r -> MST ())
      -> (a -> b -> c -> d -> r) -> MST ()
exOP4 pop1 pop2 pop3 pop4 push op
  = (op' <$> pop4 <*> pop3 <*> pop2 <*> pop1) >>= push
    where
      op' z y x w = op w x y z

-- --------------------

exOP5 :: MST a -> MST b -> MST c -> MST d -> MST e
      -> (r -> MST ())
      -> (a -> b -> c -> d -> e -> r) -> MST ()
exOP5 pop1 pop2 pop3 pop4 pop5 push op
  = (op' <$> pop5 <*> pop4 <*> pop3 <*> pop2 <*> pop1) >>= push
    where
      op' z y x w v = op v w x y z

-- --------------------
-- short cuts
--
-- int ops

exInt1 :: (Int -> Int) -> MST ()
exInt1 = exOP1 popInt pushInt

exInt2 :: (Int -> Int -> Int) -> MST ()
exInt2 = exOP2 popInt popInt pushInt

-- float ops

exFloat2' :: (c -> MST ())
          ->(Double -> Double -> c) -> MST ()
exFloat2' = exOP2 popFloat popFloat

exFloat2 :: (Double -> Double -> Double) -> MST ()
exFloat2 = exFloat2' pushFloat

exFloat2Int :: (Double -> Double -> Int) -> MST ()
exFloat2Int = exFloat2' pushInt

-- picture ops

exPic1 :: (Picture -> Picture) -> MST ()
exPic1 = exOP1 popPic pushPic

exPic2' :: (c -> MST ())
          ->(Picture -> Picture -> c) -> MST ()
exPic2' = exOP2 popPic popPic

exPic2 :: (Picture -> Picture -> Picture) -> MST ()
exPic2 = exPic2' pushPic

exPicInt2 :: (Int -> Int -> Picture -> Picture) -> MST ()
exPicInt2 = exPicInt2' . flipPII
  where
    exPicInt2' :: (Picture -> Int -> Int -> Picture) -> MST ()
    exPicInt2' = exOP3  popPic popInt popInt pushPic

    flipPII :: (Int -> Int -> Picture -> Picture)
            -> (Picture -> Int -> Int -> Picture)
    flipPII f p w h = f w h p

exPicPart :: (Int -> Picture -> [Picture]) -> MST ()
exPicPart f = exOP2 popPic (popInt >>= checkGT0) pushListPic
                    (flip f)

-- ----------------------------------------

exSVC :: Subroutine -> MST ()
exSVC "write"
  = do s <- popString
       io $ putStr s
       pushUndef

exSVC "writeln"
  = do s <- popString
       io $ putStrLn s
       pushUndef

exSVC "getArgs"
  = do argl <- io getArgs
       pushListString $ drop 2 argl

exSVC "abort"
  = throw programAborted

exSVC "exit"
  = throw programTerminated

exSVC "load"
  = do fn <- popString
       p  <- io $ readPictureFile fn
       pushPic p

exSVC "store"
  = do fn <- popString
       p  <- popPic
       io $ writePictureFile fn p
       pushUndef

exSVC "dump"
  = trc showMS >> pushUndef

exSVC sub
  = throw $ "unimplemented system call: " ++ sub

-- ----------------------------------------
