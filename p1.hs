{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

import Data.Maybe
import Data.Char (ord)
import System.IO
import System.Exit
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (runReaderT)

type MCharReader a = (Monad m, MonadTrans mr, MonadReader (m (Maybe Char)) (mr m)) => mr m a

sumOfArithmeticSequence first step n = n * (2 * first + step * (n - 1)) `div` 2

sumOfMultiplesOf3And5BelowInc :: Integer -> Integer
sumOfMultiplesOf3And5BelowInc n = (partialSum 3) + (partialSum 5) - (partialSum (3 * 5)) where
  partialSum x = sumOfArithmeticSequence x x (n `div` x)

processLine :: (MonadTrans mr, MonadReader (IO (Maybe Char)) (mr IO)) => mr IO (IO Bool, Maybe Char)
processLine = do
  (maybeN, maybeChar) <- tryReadNextUInt
  case maybeN of
    Just n -> do
      lift $ putStrLn $ show $ sumOfMultiplesOf3And5BelowInc (n - 1)
      return (return True, maybeChar)
    _ -> return (return False, maybeChar)

main :: IO ()
main = do
  (maybeN, maybeChar) <- runReaderT tryReadNextUInt maybeGetCharFromStdin
  case maybeN of
    Just n -> do
      runPrependedT (repeat n) (return maybeChar, maybeGetCharFromStdin)
      return ()
    _ -> do
      hPutStrLn stderr "No N"
      exitWith (ExitFailure 1)
  where
    repeat n
      | n <= 0 = return ()
      | otherwise = do
          (ioBool, newMaybeChar) <- processLine
          processed <- liftIO ioBool
          if processed then
            repeat (n - 1)
          else do
            liftIO $ do
              hPutStrLn stderr "Bad line"
              exitWith (ExitFailure 2)

--
skipAllNonDigits :: MCharReader (Maybe Char)
skipAllNonDigits = do
  maybeCharM <- ask
  maybeChar <- lift maybeCharM
  case maybeChar of
    Just c -> do
      if isDigit c
      then return $ Just c
      else skipAllNonDigits
    _ -> return Nothing

tryReadNextUInt :: MCharReader (Maybe Integer, Maybe Char)
tryReadNextUInt = do
  maybeChar <- skipAllNonDigits
  case maybeChar of
    Just c -> do
      readUInt (parseUInt parseUIntInitState c)
    _ -> return (Nothing, Nothing)

readUInt :: ParseUIntParseState -> MCharReader (Maybe Integer, Maybe Char)
readUInt ps = do
  maybeCharM <- ask
  maybeChar <- lift maybeCharM
  case maybeChar of
    Just c -> do
      let newPs = parseUInt ps c
      case newPs of
        ParseUIntParseStateFailed -> return (finishParseUInt ps, Just c)
        _ -> readUInt newPs
    _ -> return (finishParseUInt ps, Nothing)

-- numbers parsing
data ParseUIntParseState =
  ParseUIntParseStateInit | ParseUIntParseStateParsing Integer | ParseUIntParseStateFailed deriving Show

parseUIntInitState = ParseUIntParseStateInit

finishParseUInt (ParseUIntParseStateParsing n) = Just n
finishParseUInt _ = Nothing

parseUInt :: ParseUIntParseState -> Char -> ParseUIntParseState
parseUInt ParseUIntParseStateFailed _ = ParseUIntParseStateFailed
parseUInt s c
  | '0' <= c && c <= '9' = (ParseUIntParseStateParsing (oldN + toInteger (ord c - ord '0')))
  | otherwise = ParseUIntParseStateFailed
    where
      oldN = case s of
        ParseUIntParseStateInit      -> 0
        ParseUIntParseStateParsing n -> n * 10

-- util
($>) :: a -> (a -> b) -> b
($>) a f = f a

doWhileM :: (Monad m) => (s -> m (Bool, s)) -> s -> m s
doWhileM f s = do
  (continue, newS) <- f s
  if continue then
    doWhileM f newS
  else
    return newS

andM :: (Monad m) => m Bool -> m Bool -> m Bool
andM a b = a >>= (\x ->
  if x then b
  else return False)

notM :: (Monad m) => m Bool -> m Bool
notM a = a >>= (\x -> return $ not x)

isCanReadStdin = (hIsOpen stdin `andM` (notM isEOF))

maybeGetCharFromStdin = do
  canGetChar <- isCanReadStdin
  if canGetChar then Just `liftM` getChar else return Nothing

isDigit c
  | '0' <= c && c <= '9' = True
  | otherwise = False

-- pure wizardy
newtype PrependedT r m a = PrependedT ( ((Either (r, r) r) -> m (a, Either (r, r) r)) )
runPrependedT (PrependedT g) p = g $ Left p
continuePrependedT (PrependedT g) p = g p

instance Monad m => Monad (PrependedT r m) where
  (>>=) (PrependedT g) f = PrependedT q where
    q p = s
      where
        mp = g p
        s = do
          (a, newP) <- mp
          let PrependedT newG = f a
          newG newP
  return a = PrependedT $ (\p -> return (a, p))

instance MonadIO (PrependedT r IO) where
  liftIO io = PrependedT (\p -> io >>= (\a -> return (a, p)))
instance MonadTrans (PrependedT r) where
  lift x = PrependedT (\p -> x >>= (\a -> return (a, p)))

instance Monad m => MonadReader r (PrependedT r m) where
  ask = PrependedT g where
    g (Left (prepended, rest)) = return (prepended, Right rest)
    g (Right rest) = return (rest, Right rest)
  local f (PrependedT g) = PrependedT newG where
    newG p = s where
      mp = g p
      s = do
        (a, newP) <- mp
        let mappedP = case newP of
              Left (prepended, rest) -> Left (f prepended, f rest)
              Right rest -> Right $ f rest
        return (a, mappedP)
