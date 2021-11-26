{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import Lib
import System.IO
import Data.Functor.Identity
import Debug.Trace
import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.ByteString as BS

import qualified Data.Map as M
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class

data AIState = AIState
  { turn :: Turns
  }

initialState :: AIState
initialState = AIState
  { turn = 0
  }

type Log = [String]

logic :: GameState -> State AIState ([Order], Log)
logic st = do
  AIState current_turn <- get
  put (AIState (current_turn + 1))
  return (concatMap attackAllNeighbours (availableTargets st), dummyLog st)

dummyLog :: GameState -> Log
dummyLog st = ["We have " ++ show (length (ourPlanets st)) ++ " planets!"]

ourPlanet :: Planet -> Bool
ourPlanet (Planet (Owned Player1) _ _) = True
ourPlanet _ = False

ourPlanets :: GameState -> Planets
ourPlanets (GameState ps _ _)
  = M.filter ourPlanet ps

availableTargets :: GameState -> [(Planet, Wormholes)]
availableTargets st
  = map (\(pId, p) -> (p, wormholesFrom (Source pId) st)) (M.assocs (ourPlanets st))

attackAllNeighbours :: (Planet, Wormholes) -> [Order]
attackAllNeighbours (Planet _ (Ships s) _, ws)
  | null ws = []
  | otherwise
      = let each = max 0 (s `div` length ws)
        in if each > 0 then map (\(wId, _) -> Order wId (Ships each)) (M.assocs ws) else []

main :: IO ()
main = do
  hSetBinaryMode stdout True
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  contents <- incrementalExample <$> B.getContents
  flip evalStateT initialState $ forM_ contents $ \ms -> do
    Turns t <- gets turn
    (orders, logs) <- mapStateT (return @IO . runIdentity) (logic ms)
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
