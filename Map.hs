{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImplicitParams #-}

module Map where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.IORef
import           Data.Coerce
import           Data.List (sortOn, unfoldr, sort, nubBy)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           System.IO (Handle)
import           System.Random
import qualified Data.Binary as B
import           GHC.Generics
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder

import           Lib

type Point = (Int, Int)

type PPlanet = (Point, Planet)

randomCoordinate :: (?radius :: Int) => IO Point
randomCoordinate
  = (,) <$> randomRIO (0, ?radius) <*> randomRIO (- ?radius, ?radius)

-- No reason for this to be in IO, but it is
tooClose :: [PPlanet] -> Point -> Growth -> IO Bool
tooClose ps p (Growth g) = do
  let multiplier = 0.9
  cs <- forM ps $ \ (p', Planet _ _ (Growth g')) -> do
    let
        dist = distance p p'
        threshold = multiplier * (sqrt (fromIntegral g) + sqrt (fromIntegral g'))
    return $ dist < threshold || dist <= 5
  return (any id cs)

randomPlanet :: (?radius :: Int) => [PPlanet] -> IO PPlanet
randomPlanet ps = do
  c      <- randomCoordinate
  ships  <- Ships <$> randomRIO (50,150)
  growth <- Growth <$> randomRIO (1,5)
  too_close <- tooClose ps c growth
  if not too_close && inMap c then do
    return (c, Planet Neutral ships growth)
  else randomPlanet ps

mirrorPlanet :: PPlanet -> PPlanet
mirrorPlanet ((x, y), Planet o s g)
  = ((-x, -y), Planet (mirrorOwner o) s g)
  where mirrorOwner Neutral = Neutral
        mirrorOwner (Owned Player1) = Owned Player2
        mirrorOwner (Owned Player2) = Owned Player1

-- generates random planets for player 1
randomPlanets :: (?radius :: Int) => IO (PPlanet, [PPlanet])
randomPlanets = do
  (c, Planet _ s _) <- randomPlanet []
  let home1 = (c, Planet (Owned Player1) s (Growth 5))

  pref <- newIORef @[PPlanet] []

  replicateM_ (?radius `div` 2) $ do
    ((x, y), p) <- readIORef pref >>= randomPlanet
    modifyIORef pref (\ps -> ((x, y), p) : ps)
  ps <- readIORef pref
  return (home1, ps)

type RenderableMap = (Map PlanetId Point, Planets, Wormholes)

randomMap :: (?radius :: Int) => StdGen -> IO RenderableMap
randomMap g = do
  old_gen <- getStdGen
  setStdGen g
  ((c, home1), ps) <- randomPlanets
  let ps' = sortOn (\(c', _) -> distance c c') ps
      planets1_ :: [(PlanetId, PPlanet)]
      planets1_ = zip (map PlanetId [1..]) ps'

  (wormholes1, planets1) <- foldM
    (\(wormholes, connected) target@(target_id, (target_pos, _)) -> do
        let (source_id, (source_pos, _))
              = head (sortOn (\(_, (c', _)) -> distance target_pos c') connected)
            turns  = Turns (round (distance source_pos target_pos))
            wormhole = Wormhole (Source source_id) (Target target_id) turns
        return (wormhole : wormholes, target : connected)
    )
    ([], [(PlanetId 0, (c, home1))]) planets1_

  let
    home2_id
      = length planets1

    twinId :: PlanetId -> PlanetId
    twinId (PlanetId p)
      = PlanetId ((p+home2_id) `mod` (2*home2_id))

    wormholes2
      = map (shiftWormhole home2_id) wormholes1
    planets2
      = mirrorPlanets home2_id planets1
    planets' :: Map PlanetId (Point, Planet)
    planets'
      = M.fromList (planets1 ++ planets2)

    planets_list = M.assocs planets'
    planets_count = length planets_list

  (battle_wormholes :: [Wormhole]) <- fmap concat . forM planets_list $ \(pId, (pos, _)) -> do
    roll <- randomRIO @Int (1, 6)
    if roll > 2
      then do
        index <- randomRIO (0, planets_count - 1)
        let (pId', (pos', _)) = planets_list !! index
            dis = distance pos pos'
        if dis <= 3 then
          return []
        else do
            let turns = Turns (floor dis)
            return [ Wormhole (Source pId) (Target pId') turns
                   , Wormhole (Source pId') (Target pId) turns
                   , Wormhole (Source (twinId pId)) (Target (twinId pId')) turns
                   , Wormhole (Source (twinId pId')) (Target (twinId pId)) turns
                   ]
      else
        return []

  let wormholes
       = wormholes1 ++ wormholes2 ++ battle_wormholes

  new_ws <- forM (M.toList planets') $ \(pId, (pos, _)) -> do
    let
      pId' = twinId pId
      twinPos = fst (planets' M.! pId')
      turns = Turns (floor (distance pos twinPos))
    if isSink pId wormholes || isSource pId wormholes
      then return [ Wormhole (Source pId) (Target pId') turns
                  , Wormhole (Source pId') (Target pId) turns
                  ]
      else return []

  let same_wh (Wormhole (Source s) (Target t) _) (Wormhole (Source s') (Target t') _)
        = s == s' && t == t'
  let ws_no_sinks = wormholes ++ (nubBy same_wh $ concat new_ws)

  setStdGen old_gen

  return ( M.map fst planets'
         , M.map snd planets'
         , M.fromList (zip (map WormholeId [0..]) ws_no_sinks))

  where isSink :: PlanetId -> [Wormhole] -> Bool
        isSink pId ws = null (filter ((==pId) . source) ws)

        isSource :: PlanetId -> [Wormhole] -> Bool
        isSource pId ws = null (filter ((==pId) . target) ws)

        source :: Wormhole -> PlanetId
        source (Wormhole (Source s) _ _) = s

        target :: Wormhole -> PlanetId
        target (Wormhole (Source s) _ _) = s

        shiftPlanetId :: Int -> PlanetId -> PlanetId
        shiftPlanetId by (PlanetId pId) = PlanetId (by + pId)

        shiftWormhole :: Int -> Wormhole -> Wormhole
        shiftWormhole by (Wormhole (Source s) (Target t) turns)
          = Wormhole (Source (shiftPlanetId by s)) (Target (shiftPlanetId by t)) turns

        mirrorPlanets :: Int -> [(PlanetId, PPlanet)] -> [(PlanetId, PPlanet)]
        mirrorPlanets by = map (\(pId, planet) -> (shiftPlanetId by pId, mirrorPlanet planet))

distance :: Point -> Point -> Double
distance (x, y) (x', y')
  = let dx = x - x'
        dy = y - y'
    in sqrt (fromIntegral (dx^2 + dy^2))

--------------------------------------------------------------------------------
-- Render

newtype Render a = Render { runRender :: State Builder a }
  deriving (Functor, Applicative, Monad)

draw :: Handle -> Render a -> IO a
draw h (Render st) = do
  let (a, s) = runState st mempty
  hPutBuilder h s
  return a

append :: String -> Render ()
append str = Render (modify (<> string7 str))

appendLn :: String -> Render ()
appendLn str = Render (modify (\s -> s <> string7 str <> char7 '\n'))

setNeutralCol :: Render ()
setNeutralCol =
  resetCol
  -- append "\x1b[30m"

setP1Col :: Render ()
setP1Col = do
  append "\x1b[41m"
  append "\x1b[97m"

setP2Col :: Render ()
setP2Col = do
  append "\x1b[42m"
  append "\x1b[97m"

resetCol :: Render ()
resetCol =
  append "\x1b[0m"

displayPlanet :: Planet -> Render ()
displayPlanet (Planet o (Ships s) (Growth g)) = do
  let playerCol = case o of
        Neutral       -> setNeutralCol
        Owned Player1 -> setP1Col
        Owned Player2 -> setP2Col
  playerCol
  append "\ESC7"
  append $ "    "
  append "\ESC8"
  append $ "[+" ++ show g ++ "]"
  append "\ESC8"
  append "\ESC[1B"
  append $ "    "
  append "\ESC8"
  append "\ESC[1B"
  append $ show s
  resetCol

drawBG :: (?radius :: Int) => Render ()
drawBG = do
  moveCursor (1,1)
  forM_ [- ?radius.. ?radius] $ \y -> do
    forM_ [- ?radius.. ?radius] $ \x -> do
      if not (inMap (x, y))
        then append "."
        else append " "
      append " "
    appendLn ""
  resetCol
  moveCursor (1,1)

mapToScreen :: (?radius :: Int) => Point -> Point
mapToScreen (x, y) = ((?radius + x)*2 + 1, ?radius + y + 1)

moveCursor :: Point -> Render ()
moveCursor (x, y) = 
  append $ "\ESC[" ++ show y  ++ ";" ++ show x ++ "H"

drawPlanets :: (?radius :: Int) => RenderableMap -> Render ()
drawPlanets (positions, planets, _) = do
  forM_ (M.assocs positions) $ \ (pId, c) -> do
    moveCursor (mapToScreen c)
    displayPlanet (fromJust (M.lookup pId planets))

drawMap :: (?radius :: Int) => RenderableMap -> Fleets -> Render ()
drawMap rm@(positions, _, wormholes) fleets = do

  forM_ (M.elems wormholes) $ \(Wormhole (Source s) (Target t) _) -> do
    let c_s = mapToScreen $ fromJust (M.lookup s positions)
        c_t = mapToScreen $ fromJust (M.lookup t positions)
    drawLine c_s c_t

  forM_ fleets $ \(Fleet p _ wId (Turns remaining)) -> do
    let Wormhole (Source s) (Target t) (Turns total) = fromJust (M.lookup wId wormholes)
        c_s = mapToScreen $ fromJust (M.lookup s positions)
        c_t = mapToScreen $ fromJust (M.lookup t positions)
    drawFleet p c_s c_t (fromIntegral (total - remaining) / fromIntegral total)

  drawPlanets rm

  resetCol
  moveCursor (1, ?radius * 2 + 1)

inMap :: (?radius :: Int) => Point -> Bool
inMap (x, y) = x^2 + y^2 <= ?radius ^2

drawFleet :: Player -> Point -> Point -> Double -> Render ()
drawFleet player p1 p2 progress = do
  let ps = line p1 p2
  setPCol player
  let (x, y) = ps !! (max 0 (floor $ (fromIntegral $ length ps) * progress))
  moveCursor (x, y)
  append " "
  where setPCol Player1 = setP1Col
        setPCol Player2 = setP2Col

drawLine :: Point -> Point -> Render ()
drawLine p1 p2 = do
  append "\ESC[90m"
  let ps = line p1 p2
  forM_ ps $ \(x, y) -> do
    moveCursor (x, y)
    append "."
  resetCol

-- from https://wiki.haskell.org/Bresenham%27s_line_drawing_algorithm (tweaked)
line :: Point -> Point -> [Point]
line pa@(xa,ya) pb@(xb,yb)
  = final . map maySwitch . unfoldr go $ (x1,y1,0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else id
    [(x1,y1),(x2,y2)] = sort [maySwitch pa, maySwitch pb]
    final = if maySwitch pa < maySwitch pb then id else reverse
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, err)
        | xTemp > x2 = Nothing
        | otherwise  = Just ((xTemp, yTemp), (xTemp + 1, newY, newErr))
        where
          tempError = err + deltay
          (newY, newErr) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)
