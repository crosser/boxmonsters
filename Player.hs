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
limX = (hvsize-1.0, 1.0-hvsize)
limY = (hvsize-1.0, 1.0-hvsize)

--launch :: (Monoid e) => Wire s e m LocVel (Event LocVel)
--launch = became firePressed
{-
shooting = hold (isKeyDown ENTER) >>> (once --> coolDown >>> shooting)
  where
    coolDown =
      arr head . multicast [ after 0.05, hold (not . isKeyDown ENTER) ]
-}

aboveLo :: (Monoid e) => Wire s e m (Inputs, (Bool, Bool)) (Inputs, (Bool, Bool))
aboveLo = when $ \(_, (lo, _ )) -> lo
belowHi :: (Monoid e) => Wire s e m (Inputs, (Bool, Bool)) (Inputs, (Bool, Bool))
belowHi = when $ \(_, (_ , hi)) -> hi

movecmd :: (Inputs -> Steer) -> Steer -> (Inputs, (Bool, Bool)) -> Bool
movecmd xy decrincr (inputs, (_, _)) = (xy inputs) == decrincr

velocity :: (MonadFix m, Monoid e)
         => (Inputs -> Steer)
         -> Wire s e m (Inputs, (Bool, Bool)) Double
velocity xy = pure (-speed) . when (movecmd xy Decr) . aboveLo
          <|> pure ( speed) . when (movecmd xy Incr) . belowHi
          <|> pure (   0.0) -- . when (movecmd xy Stay)

location :: (HasTime t s, MonadFix m)
         => (Double, Double)
         -> Wire s () m Double (Double, (Bool, Bool))
location lim = clamp lim . integral 0
  where
    clamp :: (Double, Double) -> Wire s e m Double (Double, (Bool, Bool))
    clamp lim = mkPure_ $ clamp' lim
    clamp' :: (Double, Double) -> Double -> Either e (Double, (Bool, Bool))
    clamp' (lo, hi) x
      | x < lo    = Right (lo, (False, True))
      | x > hi    = Right (hi, (True, False))
      | otherwise = Right (x,  (True, True))

-- | Produce (location, velocity) tuple, honoring limits

axis :: (HasTime t s, MonadFix m)
     => (Inputs -> Steer)
     -> (Double, Double)                -- limits
     -> Wire s () m Inputs (Double, Double) -- (location, velocity) for axis
axis xy lims = proc inputs -> do
  rec (pos, lim) <- location lims -< vel
      vel <- velocity xy -< (inputs, lim)
  returnA -< (pos, vel)

playerWire :: (HasTime t s, MonadFix m) => Wire s () m Inputs Player
playerWire = Player <$> locvel <*> never . locvel -- (now . locvel . (when firePressed))
  where
    locvel :: (HasTime t s, MonadFix m) => Wire s () m Inputs LocVel
    locvel = (,) <$> loc3 <*> vel3
    loc3 :: (HasTime t s, MonadFix m) => Wire s () m Inputs Vec3
    loc3 = (,,) <$> (fst <$> (axis steerX limX))
                <*> (fst <$> (axis steerY limY))
                <*> (pure 0.5)
    vel3 :: (HasTime t s, MonadFix m) => Wire s () m Inputs Vec3
    vel3 = (,,) <$> (snd <$> (axis steerX limX))
                <*> (snd <$> (axis steerY limY))
                <*> (pure 0.0)

-- Rendering Player. Because it is not visible, the "rendering" is
-- in fact setting up the scene (box, perspective and crosshair).

renderBox :: IO ()
renderBox =
  mapM_ (renderPrimitive LineStrip . mapM_ vertex3d)
    [ [((-1), (-1), 0), ((-1), (-1), 1), ((-1),   1 , 1)]
    , [((-1),   1 , 0), ((-1),   1 , 1), (  1 ,   1 , 1)]
    , [(  1 ,   1 , 0), (  1 ,   1 , 1), (  1 , (-1), 1)]
    , [(  1 , (-1), 0), (  1 , (-1), 1), ((-1), (-1), 1)]
    ]

renderXHair :: Double -> Double -> IO ()
renderXHair x y =
  mapM_ (renderPrimitive Lines . mapM_ vertex3d)
    [ [((x-0.01),      y , 0.5), ((x-0.02),      y , 0.5)]
    , [((x+0.01),      y , 0.5), ((x+0.02),      y , 0.5)]
    , [(     x , (y-0.01), 0.5), (     x , (y-0.02), 0.5)]
    , [(     x , (y+0.01), 0.5), (     x , (y+0.02), 0.5)]
    ]

boxdepth :: IO (GLmatrix GLfloat)
boxdepth = newMatrix RowMajor [ 1, 0, 0, 0
                              , 0, 1, 0, 0
                              , 0, 0, 0, 0
                              , 0, 0, 2, 0
                              ]

boxshift :: Double -> Double -> IO (GLmatrix GLfloat)
boxshift x y = newMatrix RowMajor [ 1, 0, 0, -(realToFrac x :: GLfloat)
                                  , 0, 1, 0, -(realToFrac y :: GLfloat)
                                  , 0, 0, 1, 0
                                  , 0, 0, 0, 1
                                  ]

renderPlayer :: (Double, Double) -> Player -> IO ()
renderPlayer _ (Player ((x, y, _), _) _) = do
  matrixMode $= Projection
  loadIdentity
  boxdepth >>= multMatrix
  boxshift x y >>= multMatrix
  color4d (1, 1, 1, 1)
  renderBox

renderHUD :: (Double, Double) -> Player -> IO ()
renderHUD _ (Player ((x, y, _), _) _) = do
  color4d (0, 0, 0, 1)
  renderXHair x y
