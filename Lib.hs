
{-#  OPTIONS_GHC -Wall  #-}
{-#  OPTIONS_GHC -Wno-unused-matches  #-}
{-#  OPTIONS_GHC -Wno-name-shadowing  #-}
{-#  OPTIONS_GHC -Wno-incomplete-patterns  #-}

{-#  LANGUAGE DeriveGeneric  #-}
{-#  LANGUAGE FlexibleInstances  #-}
{-#  LANGUAGE ScopedTypeVariables  #-}
{-#  LANGUAGE FunctionalDependencies  #-}
{-#  LANGUAGE GeneralizedNewtypeDeriving  #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE InstanceSigs  #-}
{-#  LANGUAGE UndecidableInstances  #-}
{-#  LANGUAGE TypeApplications  #-}

module Lib
  ( Edge (..)
  , GameState (..)
  , Graph (..)
  , Growth (..)
  , Order (..)
  , Owner (..)
  , Path (..)
  , Planet (..)
  , PlanetId (..)
  , Planets
  , Player (..)
  , Ships (..)
  , Source (..)
  , Target (..)
  , Turns (..)
  , Fleet (..)
  , Fleets
  , Wormholes
  , wormholesFrom
  , wormholesTo
  , Wormhole (..)
  , WormholeId (..)
  , shortestPaths
  , PQueue (..)
  , lt
  , gt
  , lte
  , eq
  , maxBy
  , tabulate
  ) where

import Prelude hiding (maximum)

import Control.DeepSeq
import Data.Coerce (coerce)

import Data.Array
import Data.List (unfoldr, maximumBy, nub, sortBy, (\\))
import Data.Map (Map)
import qualified Data.Map as M

import qualified Data.Binary as B
import           GHC.Generics

data Player = Player1 | Player2

data Planet = Planet Owner Ships Growth
newtype Ships = Ships Int
newtype Growth = Growth Int

data Owner = Neutral | Owned Player

newtype PlanetId = PlanetId Int

type Planets = Map PlanetId Planet

data Wormhole = Wormhole Source Target Turns

newtype Source = Source PlanetId
newtype Target = Target PlanetId
newtype Turns  = Turns Int

newtype WormholeId = WormholeId Int

type Wormholes = Map WormholeId Wormhole

data Fleet = Fleet Player Ships WormholeId Turns

type Fleets = [Fleet]

data GameState = GameState Planets Wormholes Fleets

data Order = Order WormholeId Ships

tabulate :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate (u,v) f = array (u,v) [ (i, f i) | i <- range (u, v)]

wormholesFrom :: Source -> GameState -> Wormholes
wormholesFrom pId (GameState _ ws _)
  = M.filter (\(Wormhole s _ _) -> s == pId) ws

wormholesTo :: Target -> GameState -> Wormholes
wormholesTo pId (GameState _ ws _)
  = M.filter (\(Wormhole _ t _) -> t == pId) ws

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y = case compare (f x) (f y) of
    GT -> x
    _  -> y

type Weight = Integer

class Eq v => Edge e v | e -> v where
  source :: e -> v
  target :: e -> v
  weight :: e -> Weight

instance Edge (String, String, Integer) String where
  source (s, _, _) = s
  target (_, t, _) = t
  weight (_, _, i) = i

instance Edge Wormhole PlanetId where
  source (Wormhole (Source s) _ _)    = s
  target (Wormhole _ (Target t) _)    = t
  weight (Wormhole _ _ (Turns turns)) = toInteger turns

instance Edge (WormholeId, Wormhole) PlanetId where
  source (_, w) = source w
  target (_, w) = target w
  weight (_, w) = weight w

data Path e = Path Weight [e]

pathFromEdge :: Edge e v => e -> Path e
pathFromEdge e = Path (weight e) [e]

extend :: Edge e v => Path e -> e -> Path e
extend (Path _ []) _ = error "extend: Empty path"
extend (Path d (e:es)) e'
  | target e == source e' = Path (d + weight e') (e':e:es)
  | otherwise = error "extend: Incompatible endpoints"

instance Edge e v => Edge (Path e) v where
  source (Path _ es) = source (last es)
  target (Path _ es) = target (head es)
  weight (Path w _)  = w

class Edge e v => Graph g e v | g -> e where
  vertices  :: g -> [v]
  edges     :: g -> [e]
  edgesFrom :: g -> v -> [e]
  edgesTo   :: g -> v -> [e]
  velem     :: v -> g -> Bool
  eelem     :: e -> g -> Bool

instance (Eq e, Edge e v) => Graph [e] e v where
  vertices es = nub (map source es ++ map target es)
  edges es    = es
  edgesFrom es v = [ e | e <- es, v == source e ]
  edgesTo   es v = [ e | e <- es, v == target e ]
  velem v es = v `elem` vertices es
  eelem v es = v `elem` edges es

instance Graph GameState (WormholeId, Wormhole) PlanetId where
  vertices (GameState ps _ _) = M.keys ps
  edges    (GameState _ ws _) = M.assocs ws
  edgesTo   st pId = M.toList (wormholesTo (Target pId) st)
  edgesFrom st pId = M.toList (wormholesFrom (Source pId) st)
  velem pId      (GameState ps _ _) = M.member pId ps
  eelem (wId, _) (GameState _ ws _) = M.member wId ws

lt :: (a -> a -> Ordering) -> (a -> a -> Bool)
lt cmp x y = cmp x y == LT

gt :: (a -> a -> Ordering) -> (a -> a -> Bool)
gt cmp x y = cmp x y == GT

lte :: (a -> a -> Ordering) -> (a -> a -> Bool)
lte cmp x y = cmp x y /= GT

eq :: (a -> a -> Ordering) -> (a -> a -> Bool)
eq cmp x y = cmp x y == EQ

class PQueue pqueue where
  toPQueue   :: (a -> a -> Ordering) -> [a] -> pqueue a
  toPQueue cmp xs = foldr insert (empty cmp) xs
 
  fromPQueue :: pqueue a -> [a]
  fromPQueue = unfoldr unqueue
      where
        unqueue q
          | isEmpty q = Nothing
          | otherwise = Just (detach q)

  priority :: pqueue a -> (a -> a -> Ordering)

  empty :: (a -> a -> Ordering) -> pqueue a
  isEmpty :: pqueue a -> Bool

  insert :: a -> pqueue a -> pqueue a

  extract :: pqueue a -> a
  discard :: pqueue a -> pqueue a
  detach  :: pqueue a -> (a, pqueue a)
  detach q = (extract q, discard q)

data PList a = PList (a -> a -> Ordering) [a]

instance PQueue PList where

  toPQueue cmp xs = PList cmp (sortBy cmp xs)

  fromPQueue (PList _ xs) = xs

  empty cmp = PList cmp []

  isEmpty (PList _ xs) = null xs

  priority (PList cmp _) = cmp

  insert x (PList cmp []) = PList cmp [x]
  insert x ps@(PList cmp xs)
    | x <= y    = cons x ps
    | otherwise = cons y (insert x ys)
    where (<=) = lte cmp
          (y, ys) = detach ps
          cons x (PList cmp xs) = PList cmp (x:xs)

  extract (PList cmp (x:xs)) = x

  discard (PList cmp (x:xs)) = PList cmp xs

cmpPath :: Path v -> Path v -> Ordering
cmpPath (Path d _) (Path d' _) = compare d d'

shortestPaths :: forall g e v. Graph g e v => g -> v -> [Path e]
shortestPaths g v = dijkstra g (vertices g \\ [v]) ps
 where
  ps :: PList (Path e)
  ps = toPQueue cmpPath (map pathFromEdge (edgesFrom g v))

dijkstra :: (Graph g e v, PQueue pqueue) =>
  g -> [v] -> pqueue (Path e) -> [Path e]
dijkstra g [] ps = []
dijkstra g us ps
  | isEmpty ps  = []
  | t `elem` us = p : dijkstra g (us \\ [t])
                                 (foldr insert ps' (map (extend p) (edgesFrom g t)))
  | otherwise  = dijkstra g us ps'
  where
    (p, ps') = detach ps
    t = target p

deriving instance Eq Player
deriving instance Show Player
deriving instance Read Player
deriving instance Generic Player
instance B.Binary Player
deriving instance Eq Owner
deriving instance Show Owner
deriving instance Read Owner
deriving instance Generic Owner
instance B.Binary Owner
deriving instance Show Planet
deriving instance Read Planet
deriving instance Generic Planet
instance B.Binary Planet
deriving instance Show Fleet
deriving instance Read Fleet
deriving instance Generic Fleet
instance B.Binary Fleet

deriving instance Show Wormhole
deriving instance Read Wormhole
deriving instance Generic Wormhole
instance B.Binary Wormhole

deriving instance Show Order
deriving instance Read Order
deriving instance Generic Order
instance B.Binary Order
deriving instance Show GameState
deriving instance Read GameState
deriving instance Generic GameState
instance B.Binary GameState

deriving instance Ord PlanetId
deriving instance Eq PlanetId
deriving instance Num PlanetId
deriving instance B.Binary PlanetId
instance Show PlanetId where
  show (PlanetId x) = show x
instance Read PlanetId where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Turns
deriving instance Eq Turns
deriving instance Num Turns
deriving instance B.Binary Turns
instance Show Turns where
  show (Turns x) = show x
instance Read Turns where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Source
deriving instance Eq Source
deriving instance B.Binary Source
instance Show Source where
  show (Source x) = show x
instance Read Source where
  readsPrec = coerce (readsPrec @Int)

deriving instance Num Growth
deriving instance Ord Growth
deriving instance Eq Growth
deriving instance B.Binary Growth
instance Show Growth where
  show (Growth x) = show x
instance Read Growth where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ix Ships
deriving instance Num Ships
deriving instance Ord Ships
deriving instance Eq Ships
deriving instance B.Binary Ships
instance Show Ships where
  show (Ships x) = show x
instance Read Ships where
  readsPrec = coerce (readsPrec @Int)

deriving instance Ord Target
deriving instance Eq Target
deriving instance B.Binary Target
instance Show Target where
  show (Target x) = show x
instance Read Target where
  readsPrec = coerce (readsPrec @Int)

deriving instance Eq WormholeId
deriving instance Ord WormholeId
deriving instance B.Binary WormholeId
instance Show WormholeId where
  show (WormholeId x) = show x
instance Read WormholeId where
  readsPrec = coerce (readsPrec @Int)

deriving instance Eq e   => Eq (Path e)
deriving instance Read e => Read (Path e)
deriving instance Show e => Show (Path e)
instance Show a => Show (PList a) where
  show (PList _ xs) = show xs

deriving instance Generic PlanetId
deriving instance Generic WormholeId
deriving instance Generic Ships
deriving instance Generic (Path a)
 
instance NFData PlanetId
instance NFData Order
instance NFData WormholeId
instance NFData Ships
instance NFData a => NFData (Path a)
