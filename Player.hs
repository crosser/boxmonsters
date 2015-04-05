-- | Player is an Identified Flying Object steered by keys in the plane z=0

{-# LANGUAGE Arrows #-}

module Player (Player(Player), playerWire, renderPlayer, renderHUD) where

import Prelude hiding ((.), id)
import Control.Monad.Fix
import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import GLUtils
import IFOs

data Player = Player LocVel Launch

-- Functional part.

-- game constants
speed = 0.5
lspeed = 0.5
hvsize = 0.05

-- Clamping passed over from `location`
-- meaning (aboveLowLimit, belowHighLimit) for each axis
type InLims = V3 (Bool, Bool)

-- | Player velocity from inputs and clamping

velocity :: (MonadFix m, Monoid e)
         => Wire s e m (Inputs, InLims) Vel
velocity = (v3w vel1) <<^ \(inp, inlim) -> pair2v ((steer inp), inlim)
  where
  vel1 :: (Monad m, Monoid e) => Wire s e m (Steer, (Bool, Bool)) Double
  vel1 =  pure (-speed) . when (steeris Decr) . when aboveLo
      <|> pure ( speed) . when (steeris Incr) . when belowHi
      <|> pure (     0) -- . when (steer Stay)
  steeris d (s, (_, _)) = s == d
  aboveLo   (_, (x, _)) = x
  belowHi   (_, (_, x)) = x

-- | Player location from inputs and velocity
-- Output coordinates and InLims Bool pairs.

location :: (HasTime t s, MonadFix m)
         => Wire s e m (Inputs, Vel) (Loc, InLims)
location = v2pair ^<< location' <<^ \(inp, vel) -> pair2v ((norm inp), vel)
location' :: (HasTime t s, MonadFix m)
          => Wire s e m (V3 (Double, Double)) (V3 (Double, (Bool, Bool)))
location' = location1
location1 = proc (size, vel) -> do
  rawloc <- integral 0 -< vel
  clamped <- mkSF_ clamp -< (rawloc, size)
  returnA -< clamped
    where
    clamp (x, size)
      | x < lo    = (lo, (False, True))
      | x > hi    = (hi, (True, False))
      | otherwise = (x,  (True, True))
      where
      lo = hvsize - size
      hi = size - hvsize

-- | Player's (location, velocity) tuple, honoring limits

locvel :: (HasTime t s, MonadFix m)
       => Wire s e m Inputs LocVel
locvel = proc inputs -> do
  rec (loc, lim) <- location -< (inputs, vel)
      vel <- velocity -< (inputs, lim)
  returnA -< (loc, vel)

-- | Player wire, produces location and event stream of launches

playerWire :: (HasTime t s, MonadFix m) => Wire s () m Inputs Player
playerWire = Player <$> locvel <*> launch
  where
  launch = once . now . (launchlv <$> locvel) . when firePressed <|> never
  launchlv (loc, (vx, vy, _)) = (loc, (vx, vy, lspeed))

-- Rendering Player. Because it is not visible, the "rendering" is
-- in fact setting up the scene (box, perspective and crosshair).

renderBox :: Double -> Double -> IO ()
renderBox kx ky =
  mapM_ (renderPrimitive LineStrip . mapM_ vertex3d)
    [ [((-kx), (-ky), 0), ((-kx), (-ky), 1), ((-kx),   ky , 1)]
    , [((-kx),   ky , 0), ((-kx),   ky , 1), (  kx ,   ky , 1)]
    , [(  kx ,   ky , 0), (  kx ,   ky , 1), (  kx , (-ky), 1)]
    , [(  kx , (-ky), 0), (  kx , (-ky), 1), ((-kx), (-ky), 1)]
    ]

renderXHair :: Double -> Double -> IO ()
renderXHair x y =
  mapM_ (renderPrimitive Lines . mapM_ vertex3d)
    [ [((x-0.02),      y , 0.5), ((x-0.05),      y , 0.5)]
    , [((x+0.02),      y , 0.5), ((x+0.05),      y , 0.5)]
    , [(     x , (y-0.02), 0.5), (     x , (y-0.05), 0.5)]
    , [(     x , (y+0.02), 0.5), (     x , (y+0.05), 0.5)]
    ]

boxdepth :: IO (GLmatrix GLfloat)
boxdepth = newMatrix RowMajor [ 1, 0, 0, 0
                              , 0, 1, 0, 0
                              , 0, 0, 0, 0
                              , 0, 0, 2, 0
                              ]

boxshift :: Double -> Double -> IO (GLmatrix GLfloat)
boxshift x y = newMatrix RowMajor [ 1, 0, 0, -(x')
                                  , 0, 1, 0, -(y')
                                  , 0, 0, 1, 0
                                  , 0, 0, 0, 1
                                  ]
  where
    x' = realToFrac x :: GLfloat
    y' = realToFrac y :: GLfloat

boxscale :: Double -> Double -> IO (GLmatrix GLfloat)
boxscale kx ky = newMatrix RowMajor [ 1/x', 0   , 0, 0
                                    , 0   , 1/y', 0, 0
                                    , 0   , 0   , 1, 0
                                    , 0   , 0   , 0, 1
                                    ]
  where
    x' = realToFrac kx :: GLfloat
    y' = realToFrac ky :: GLfloat

renderPlayer :: (Double, Double) -> Player -> IO ()
renderPlayer (kx, ky) (Player ((x, y, _), _) _) = do
  matrixMode $= Projection
  loadIdentity
  boxscale kx ky >>= multMatrix
  boxdepth >>= multMatrix
  boxshift x y >>= multMatrix
  color4d (1, 1, 1, 1)
  lineWidth $= 1.5
  renderBox kx ky

renderHUD :: (Double, Double) -> Player -> IO ()
renderHUD _ (Player ((x, y, _), _) _) = do
  color4d (1, 0, 1, 1)
  lineWidth $= 0.5
  renderXHair x y
