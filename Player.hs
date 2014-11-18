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
hvsize = 0.05

-- | Player velocity by axis x or y (defined by X or Y).
-- Clamping passed over from `location` as (aboveLow, belowHigh) Bool pair.
-- Output velocity.

velocity :: (MonadFix m, Monoid e)
         => XorY
         -> Wire s e m (Inputs, (Bool, Bool)) Double
velocity xy = pure (-speed) . when (movecmd xy Decr)
                            . when (\(_, (lo, _ )) -> lo)
          <|> pure ( speed) . when (movecmd xy Incr)
                            . when (\(_, (_ , hi)) -> hi)
          <|> pure (     0) -- . when (movecmd (xy . steerXY) Stay)
  where
    movecmd xy decrincr (inputs, (_, _)) = (steer xy inputs) == decrincr

-- | Player location by axis x or y (defined by X or Y).
-- Input is (inputs, velocity).
-- Output coordinate and (aboveLow, belowHigh) Bool pair.

location :: (HasTime t s, MonadFix m)
         => XorY
         -> Wire s () m (Inputs, Double) (Double, (Bool, Bool))
location xy = proc (inputs, vel) -> do
  rawloc <- integral 0 -< vel
  size <- mkSF_ (nsize xy) -< inputs
  clamped <- clamp -< (rawloc, size)
  returnA -< clamped
    where
      clamp :: Wire s e m (Double, Double) (Double, (Bool, Bool))
      clamp = mkPure_ $ clamp'
      clamp' (x, size)
        | x < lo    = Right (lo, (False, True))
        | x > hi    = Right (hi, (True, False))
        | otherwise = Right (x,  (True, True))
        where
          lo = hvsize - size
          hi = size - hvsize

-- | Produce (location, velocity) tuple, honoring limits, by one axis

axis :: (HasTime t s, MonadFix m)
     => XorY
     -> Wire s () m Inputs (Double, Double) -- (location, velocity) for axis
axis xy = proc inputs -> do
  rec (pos, lim) <- location xy -< (inputs, vel)
      vel <- velocity xy -< (inputs, lim)
  returnA -< (pos, vel)

playerWire :: (HasTime t s, MonadFix m) => Wire s () m Inputs Player
playerWire = Player <$> locvel <*> launch
  where
    locvel = (,) <$> loc3 <*> vel3
    loc3 = (,,) <$> (fst <$> axis X) <*> (fst <$> axis Y) <*> (pure 0.5)
    vel3 = (,,) <$> (snd <$> axis X) <*> (snd <$> axis Y) <*> (pure 0.0)
    launch :: Wire s () m Inputs Launch
    launch = never
    -- launch = became firePressed now . locvel . (when firePressed)
    {-
    shooting = hold (isKeyDown ENTER) >>> (once --> coolDown >>> shooting)
      where
        coolDown =
          arr head . multicast [ after 0.05, hold (not . isKeyDown ENTER) ]
    -}


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
    [ [((x-0.01),      y , 0.5), ((x-0.03),      y , 0.5)]
    , [((x+0.01),      y , 0.5), ((x+0.03),      y , 0.5)]
    , [(     x , (y-0.01), 0.5), (     x , (y-0.03), 0.5)]
    , [(     x , (y+0.01), 0.5), (     x , (y+0.03), 0.5)]
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
