
{-#  LANGUAGE GeneralizedNewtypeDeriving  #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE DeriveGeneric  #-}

module Submission2 where
import Lib

  hiding (example1, example2, example3, lookupPlanet)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List (unfoldr)
import Data.List
import Data.Maybe
import Text.Printf
import Control.DeepSeq
import GHC.Generics

deriving instance (Integral Growth)
deriving instance (Enum Growth)
deriving instance (Real Growth)

data Strategy
  = Pacifist
  | ZergRush
  | PlanetRankRush
  | Skynet
  deriving (Enum, Bounded, Show, Read)

logic :: Strategy -> GameState -> AIState -> ([Order], Log, AIState)
logic strat gs ai
  = let logic' = case strat of
          Pacifist       -> pacifist
          ZergRush       -> zergRush
          PlanetRankRush -> planetRankRush
          Skynet         -> skynet
    in logic' gs ai {turn = turn ai + 1}

data AIState = AIState
  { turn :: Turns
  , rushTarget :: Maybe PlanetId
  , pr :: Maybe (PageRanks PlanetId)
  } deriving Generic
 
initialState :: AIState
initialState = AIState
  { turn = 0
  , rushTarget = Nothing
  , pr = Nothing
  }

type Log = [String]

pacifist :: GameState -> AIState -> ([Order], Log, AIState)
pacifist _ ai = ([], ["Do no harm."], ai)

enemyPlanet :: Planet -> Bool
enemyPlanet (Planet (Owned Player2) _ _) = True
enemyPlanet _                            = False

findEnemyPlanet :: GameState -> Maybe PlanetId
findEnemyPlanet (GameState ps _ _) = case M.keys (M.filter enemyPlanet ps) of
  []         -> Nothing
  (ep : eps) -> Just ep

findOtherPlanet :: GameState -> Maybe PlanetId
findOtherPlanet (GameState ps _ _) = case M.keys (M.filter (not . ourPlanet) ps) of
  []         -> Nothing
  (ep : eps) -> Just ep

send :: WormholeId -> Maybe Ships -> GameState -> [Order]
send wId ss st
  | ourPlanet srcp = [(Order wId ships)]
  | otherwise     = []
  where
    Wormhole (Source src) (Target dst) _ = lookupWormhole wId st
    srcp@(Planet _ srcShips _)           = lookupPlanet src st
    dstp@(Planet _ dstShips _)           = lookupPlanet dst st

    ships = case ss of
      Nothing -> srcShips
      _       -> minimum [fromJust ss, srcShips]

shortestPath :: PlanetId -> PlanetId -> GameState 
             -> Maybe (Path (WormholeId, Wormhole))
shortestPath src dst st
  = case filter ((== dst) . target) (shortestPaths st src) of
      []      -> Nothing
      (x : _) -> Just x

ourPlanet :: Planet -> Bool
ourPlanet (Planet (Owned Player1) _ _) = True
ourPlanet _ = False

ourPlanets :: GameState -> Planets
ourPlanets (GameState ps _ _) = M.filter ourPlanet ps

lookupWormhole :: WormholeId -> GameState -> Wormhole
lookupWormhole wId (GameState _ wormholes _)
  = wormholes M.! wId

lookupPlanet :: PlanetId -> GameState -> Planet
lookupPlanet pId (GameState planets _ _)
  = planets M.! pId

attackFromAll :: PlanetId -> GameState -> [Order]
attackFromAll targetId st@(GameState ps _ _)
  = concat (map (\wId -> send wId Nothing st) wIds)
  where
    ourPlanets = M.filter ourPlanet ps 
    paths      = map (\src -> shortestPath src targetId st) (M.keys ourPlanets)
    wIds       = map (getWormholeId . fromJust) (filter isJust paths)

    getWormholeId :: Path (WormholeId, Wormhole) -> WormholeId
    getWormholeId (Path _ ws) = let (wId, _) = last ws in wId
    
zergRush :: GameState -> AIState 
         -> ([Order], Log, AIState)
zergRush gs@(GameState planets _ _) ai = (orders, 
                                          [], 
                                          ai {rushTarget = target} )
  where
    ourPlanetId :: PlanetId -> Bool
    ourPlanetId = ourPlanet . ((flip lookupPlanet) gs)

    target 
      | isNothing (rushTarget ai) ||
        (ourPlanetId . fromJust) (rushTarget ai) = findEnemyPlanet gs
      | otherwise                                = rushTarget ai

    orders
      | isNothing target = []
      | otherwise         = attackFromAll (fromJust target) gs
      
newtype PageRank = PageRank Double
  deriving (Num, Eq, Ord, Fractional)
 
type PageRanks pageId = Map pageId PageRank

instance Show PageRank where
  show (PageRank p) = printf "%.4f" p

initPageRanks :: (Graph g e pageId, Ord pageId) 
              => g -> PageRanks pageId
initPageRanks g = M.fromList [ (p, PageRank (1 / fromIntegral n))
                             | p <- ps ]
  where ps = vertices g
        n  = length ps

example1 :: [(String, String, Integer)]
example1 = [("a","b",1), ("a","c",1), ("a","d",1),
            ("b","a",1), ("c","a",1), ("d","a",1), ("c","d",1)]

initPageRank' :: Map pageId a -> PageRanks pageId
initPageRank' m = M.map (const (1 / fromIntegral n)) m
  where
    n  = length m
    

nextPageRank :: (Ord pageId, Edge e pageId, Graph g e pageId) => 
  g -> PageRanks pageId -> pageId -> PageRank
nextPageRank g pr i = (1 - d) / n + d * sum [ pr M.! j / t j 
                                            | j <- s i ]
 where
  d   = 0.85
  n   = fromIntegral (length (vertices g))
  t j = fromIntegral (length (edgesFrom g j))
  s i = map source (edgesTo g i)

nextPageRanks :: Ord pageId => Graph g e pageId =>
  g -> PageRanks pageId -> PageRanks pageId
nextPageRanks g pr = M.mapWithKey (const . nextPageRank g pr) pr

pageRanks :: (Ord pageId, Graph g e pageId) => g -> [PageRanks pageId]
pageRanks g = iterate (nextPageRanks g) (initPageRanks g)

pageRank :: (Ord pageId, Graph g e pageId) =>
  g -> PageRanks pageId
pageRank g = pageRanks g !! 200

nextPageRank' :: (Ord pageId, Edge e pageId, Graph g e pageId) => 
  g -> PageRanks pageId -> PageRank -> pageId -> PageRank -> Maybe PageRank
nextPageRank' g pr k i pri
  | abs (pri - pri') < k  = Nothing
  | otherwise             = Just pri'
 where
   pri' = nextPageRank g pr i

nextPageRanks' :: Ord pageId => Graph g e pageId =>
  g -> PageRank -> PageRanks pageId -> Maybe (PageRanks pageId)
nextPageRanks' g k pr = case M.mapAccumWithKey nextPageRank'' True pr of
                           (True,  pr)  -> Nothing
                           (False, pr') -> Just pr'
  where
    nextPageRank'' converged i pri = case nextPageRank' g pr k i pri of
                            Nothing   -> (converged, pri)
                            Just pri' -> (False, pri')

pageRanks' :: (Ord pageId, Graph g e pageId)
  => g -> PageRank -> [PageRanks pageId]
pageRanks' g k = iterateMaybe (nextPageRanks' g k) (initPageRanks g)
 
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x = x : maybe [] (iterateMaybe f) (f x)

pageRank' :: (Ord pageId, Graph g e pageId) =>
  g -> PageRanks pageId
pageRank' g = (last . take 200) (pageRanks' g 0.0001)

example2 :: GameState
example2 = GameState planets wormholes fleets where
  planets = M.fromList
    [ (PlanetId 0, Planet (Owned Player1) (Ships 300) (Growth 7))
    , (PlanetId 1, Planet Neutral         (Ships 200) (Growth 2))
    , (PlanetId 2, Planet Neutral         (Ships 150) (Growth 3))
    , (PlanetId 3, Planet Neutral         (Ships 30)  (Growth 6))
    ]
  wormholes = M.fromList
    [ (WormholeId 0, Wormhole (Source 0) (Target 1) (Turns 1))
    , (WormholeId 1, Wormhole (Source 0) (Target 2) (Turns 1))
    , (WormholeId 2, Wormhole (Source 0) (Target 3) (Turns 1))
    , (WormholeId 3, Wormhole (Source 1) (Target 0) (Turns 1))
    , (WormholeId 4, Wormhole (Source 2) (Target 0) (Turns 1))
    , (WormholeId 5, Wormhole (Source 3) (Target 0) (Turns 1))
    , (WormholeId 6, Wormhole (Source 2) (Target 3) (Turns 1))
    ]
  fleets = []

newtype PlanetRank = PlanetRank Double
  deriving (Num, Eq, Ord, Fractional)
 
type PlanetRanks = Map PlanetId PlanetRank
 
instance Show PlanetRank where
  show (PlanetRank p) = printf "%.4f" p

initPlanetRanks :: GameState -> PlanetRanks
initPlanetRanks g = M.fromList [ (p, PlanetRank (1 / fromIntegral n))
                               | p <- ps ]
  where ps = vertices g
        n  = length ps

planetRank :: GameState -> PlanetRanks
planetRank g = planetRanks g !! 200
 
planetRanks :: GameState -> [PlanetRanks]
planetRanks g = iterate (nextPlanetRanks g) (initPlanetRanks g)

nextPlanetRanks :: GameState -> PlanetRanks -> PlanetRanks
nextPlanetRanks g pr = M.mapWithKey (const . nextPlanetRank g pr) pr

nextPlanetRank :: GameState -> PlanetRanks 
               -> PlanetId -> PlanetRank
nextPlanetRank g@(GameState planets _ _) pr i = 
 (1 - d) / n + d * sum [ pr M.! j * growth i / growths j 
                       | j <- targets i ]
 where
  d   = 0.85
  n   = fromIntegral (length planets)

  growth :: PlanetId -> PlanetRank
  growth i  = (\(Planet _ _ g) -> fromIntegral g) 
                                  (planets M.! i)
  targets :: PlanetId -> [PlanetId]
  targets i = map (\(_, (Wormhole _ (Target t) _)) -> t) (edgesFrom g i)

  sources :: PlanetId -> [PlanetId]
  sources i = map (\(_, (Wormhole (Source s) _ _)) -> s) (edgesTo g i)
 
  growths :: PlanetId -> PlanetRank
  growths j = (fromIntegral . sum)
      (map (\pId -> let (Planet _ _ g) = planets M.! pId in g) 
           (sources j))

checkPlanetRanks :: PlanetRanks -> PlanetRank
checkPlanetRanks = sum . M.elems

planetRankRush :: GameState -> AIState 
               -> ([Order], Log, AIState)
planetRankRush gs@(GameState ps _ _) ai = (orders, [], ai {pr = mpr})
  where
    mpr :: Maybe (PageRanks PlanetId)
    mpr 
      | isNothing (pr ai) = Just (pageRank' gs) 
      | otherwise         = (pr ai)

    pr'  = (fromJust mpr)

    target = foldl (\pId maxPId -> if (pr' M.! pId) > (pr' M.! maxPId)
                                   then pId 
                                   else maxPId) 
                   0 
                   (let p = M.keys (M.filter enemyPlanet ps) in case p of
                      []        -> (M.keys (M.filter (not . ourPlanet) ps)) 
                      otherwise -> p)
                   
    orders = attackFromAll target gs
    



skynet :: GameState -> AIState
       -> ([Order], Log, AIState)
skynet gs ai = gainTerritory gs ai

gainTerritory :: GameState -> AIState 
       -> ([Order], Log, AIState)
gainTerritory gs@(GameState ps _ _) ai = (os, log', ai)
  where
    -- Spread thinly to fastest growing and nearest planets
    ops = M.keys (M.filter ourPlanet ps)
    nps = M.keys (M.filter enemyPlanet ps)
    -- all shortest distances from our planets to neutral planets
    os = concat (map (perPlanetOrder nps) ops)
    log' = (map show os)

    perPlanetOrder :: [PlanetId] -> PlanetId -> [Order]
    perPlanetOrder nps src = map (\wId -> Order wId (Ships shipsPerOrder)) best_wormholes
      where
        split = 0
        best_wormholes = take 1 (map snd ((reverse . sort) (map (planetToPri src) nps)))
        (Planet _ (Ships s) _) = ps M.! src
        shipsPerOrder = s `div` (split + 1)

    planetToPri :: PlanetId -> PlanetId -> (Double, WormholeId)
    planetToPri srcId dstId = (fromIntegral g / fromIntegral w, wId) 
      where
        p = shortestPath srcId dstId gs 
        (Just (Path w ws)) = p

        (wId, wormhole@(Wormhole _ (Target pId) _)) = last ws
        (Planet _ _ (Growth g)) = ps M.! pId

deriving instance Generic PlanetRank
deriving instance Generic PageRank
 
instance NFData PageRank
instance NFData PlanetRank
