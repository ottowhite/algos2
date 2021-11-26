{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Lib (GameState, Turns (..))
import Submission2 (logic, initialState, AIState(turn), Strategy (..))
import System.IO
import Data.Functor.Identity
import Debug.Trace
import System.Environment
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.ByteString as BS

import Control.Monad.State

stateify :: (st -> (a, b, st)) -> State st (a, b)
stateify l
  = StateT $ \st ->
      let (a, b, st') = l st in Identity ((a, b), st')

main :: IO ()
main = do
  [strat] <- map (read @Strategy) <$> getArgs
  hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  contents <- incrementalExample <$> B.getContents
  flip evalStateT initialState $ forM_ contents $ \ms -> do
    Turns t <- gets turn
    (orders, logs) <- mapStateT (return @IO . runIdentity) (stateify (logic strat ms))
    liftIO $ when (not (null logs)) $ do
      traceIO ("-------------- " ++ show t ++ ": --------------")
      traceIO (unlines logs)
    let payload = B.encode orders
    liftIO (B.putStr (B.encode (B.length payload)))
    liftIO (B.putStr payload)

incrementalExample :: B.ByteString -> [GameState]
incrementalExample input0 = go decoder input0
  where
    decoder = B.runGetIncremental B.get
    go :: B.Decoder GameState -> B.ByteString -> [GameState]
    go (B.Done leftover _consumed trade) input =
      trade : go decoder (B.chunk leftover input)
    go (B.Partial k) input                     =
      go (k . takeHeadChunk $ input) (dropHeadChunk input)
    go (B.Fail _leftover _consumed msg) _input =
      error msg

takeHeadChunk :: B.ByteString -> Maybe BS.ByteString
takeHeadChunk lbs =
  case lbs of
    (B.Chunk bs _) -> Just bs
    _ -> Nothing

dropHeadChunk :: B.ByteString -> B.ByteString
dropHeadChunk lbs =
  case lbs of
    (B.Chunk _ lbs') -> lbs'
    _ -> B.Empty

