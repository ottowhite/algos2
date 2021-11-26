{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module AutoTest where

import Submission2
import Test (runTests, test, testFromFile, Test (..), Question (..))

import Lib
import Data.Map (fromList)
import qualified Data.Map as M
import Data.Coerce
import Control.Exception
import GHC.Generics
import Data.Type.Bool
import Data.List
import Data.Function

import Control.Monad
import Control.DeepSeq

import System.IO

deriving instance Num Target
deriving instance Num Source
deriving instance Num WormholeId
deriving instance Eq Order
deriving instance Ord Order

instance Read PageRank where
  readsPrec = coerce (readsPrec @Double)

instance Read PlanetRank where
  readsPrec = coerce (readsPrec @Double)

-- | Question n and its max mark
qn :: String -> Question
qn i = case lookup i [
    ("1", 10),
    ("3", 10),
    ("4", 10),
    ("5", 10),
    ("6-7", 20),
    ("8-9", 20)
    ] of
        Just n -> Question (i, n)
        Nothing -> error $ "Question Doens't exist: " ++ i

main :: IO ()
main = runTests $ do
  testFromFile "PageRank tests"  (qn "6-7") (pageRank' @PlanetId @GameState) same "tests/pageRankTests.txt"
  testFromFile "PlanetRank tests"  (qn "8-9") planetRank same "tests/planetRankTests.txt"
  testFromFile "Find enemy tests" (qn "1") findEnemyPlanet (==) "tests/findEnemyTests.txt"
  testFromFile "Attack from all tests"  (qn "4") (uncurry attackFromAll) sameOrders "tests/attackFromAllTests.txt"
  testFromFile "Send tests"  (qn "3") (uncurry3 send) sameOrders "tests/sendTests.txt"
  test "AIState rushTarget tests"  (qn "5") (const hasRushTargetField) (==) [() :=> True]

epsilon :: Fractional a => a
epsilon = 0.001

same :: (Ord k, Fractional a, Ord a) => M.Map k a -> M.Map k a -> Bool
same ps1 ps2 = and (M.map (< epsilon) (M.unionWith (\p1 p2 -> abs (p1 - p2)) ps1 ps2))

sameOrders :: [Order] -> [Order] -> Bool
sameOrders os1 os2
  = normalise os1 == normalise os2
  where normalise :: [Order] -> [Order]
        normalise os = map (foldr1 combine) (groupBy ((==) `on` wh) (sortOn wh os))
        combine :: Order -> Order -> Order
        combine (Order w s) (Order _ s') = Order w (s + s')
        wh (Order w _) = w

type family HasRushTargetField (s :: * -> *) :: Bool where
  HasRushTargetField (D1 _ x) = HasRushTargetField x
  HasRushTargetField (C1 _ x) = HasRushTargetField x
  HasRushTargetField (l :*: r) = HasRushTargetField l || HasRushTargetField r
  HasRushTargetField (l :+: r) = HasRushTargetField l && HasRushTargetField r
  HasRushTargetField (S1 ('MetaSel ('Just "rushTarget") _ _ _) (Rec0 (Maybe PlanetId))) = 'True
  HasRushTargetField _ = 'False

class KnownBool (b :: Bool) where
  boolVal :: Bool

instance KnownBool 'True where
  boolVal = True

instance KnownBool 'False where
  boolVal = False

hasRushTargetField :: Bool
hasRushTargetField = boolVal @(HasRushTargetField (Rep AIState))

uncurry3 :: (a->b->c -> d) -> (a,b,c) -> d
uncurry3 f ~(a, b, c) = f a b c
