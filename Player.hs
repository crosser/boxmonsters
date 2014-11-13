{-# LANGUAGE Arrows #-}

module Player (Player, playerWire, renderPlayer, renderHUD) where

import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import GLUtils
import IFOs

data Player = Player PosVel

-- Functional part. Player is a wire controlled by keypresses.

-- game constants
speed = 0.5
hvsize = 0.05
limX = (hvsize-1.0, 1.0-hvsize)
limY = (hvsize-1.0, 1.0-hvsize)

-- key pairs for X and Y control
kpX = (LEFT, RIGHT)
kpY = (DOWN, UP)

isKeyDown :: (Enum k, Monoid e) => k -> Wire s e IO a e
isKeyDown k = mkGen_ $ \_ -> do
  s <- getKey k
  return $ case s of
    Press   -> Right mempty
    Release -> Left  mempty

launch :: (Monoid e) => Wire s e IO e (Event e)
launch = (became (\_ -> True)) . (isKeyDown ENTER)
{-
shooting = hold (isKeyDown ENTER) >>> (once --> coolDown >>> shooting)
  where
    coolDown =
      arr head . multicast [ after 0.05, hold (not . isKeyDown ENTER) ]
-}

aboveLo :: (Monoid e) => Wire s e m (Bool, Bool) e
aboveLo = mkPure_ $ \hit -> if (fst hit) then Right mempty else Left mempty
belowHi :: (Monoid e) => Wire s e m (Bool, Bool) e
belowHi = mkPure_ $ \hit -> if (snd hit) then Right mempty else Left mempty
ignLim  :: (Monoid e) => Wire s e m (Bool, Bool) e
ignLim  = mkPure_ $ \_ -> Right mempty

velocity :: (Monoid e)
         => (SpecialKey, SpecialKey)
         -> Wire s e IO (Bool, Bool) Double
velocity kp = pure (   0.0) . isKeyDown (fst kp) . isKeyDown (snd kp) . ignLim
          <|> pure (-speed) . isKeyDown (fst kp) . aboveLo
          <|> pure ( speed) . isKeyDown (snd kp) . belowHi
          <|> pure (   0.0) . ignLim

location :: (HasTime t s)
         => (Double, Double)
         -> Wire s () IO Double (Double, (Bool, Bool))
location lim = clamp lim . integral 0
  where
    clamp :: (Double, Double) -> Wire s e m Double (Double, (Bool, Bool))
    clamp lim = mkPure_ $ clamp' lim
    clamp' :: (Double, Double) -> Double -> Either e (Double, (Bool, Bool))
    clamp' (lo, hi) x
      | x < lo    = Right (lo, (False, True))
      | x > hi    = Right (hi, (True, False))
      | otherwise = Right (x,  (True, True))

axis :: (HasTime t s)
     => (SpecialKey, SpecialKey)        -- key pair
     -> (Double, Double)                -- limits
     -> Wire s () IO a (Double, Double)   -- (location, velocity) for one axis
axis kp lims = proc _ -> do
  rec (pos, lim) <- location lims -< vel
      vel <- velocity kp -< lim
  returnA -< (pos, vel)

playerWire :: (HasTime t s) => Wire s () IO a Player
playerWire = Player <$> ((,) <$> pos3 <*> vel3)
  where
    pos3 :: (HasTime t s) => Wire s () IO a Vec3
    pos3 = (,,) <$> (fst <$> (axis kpX limX))
                    <*> (fst <$> (axis kpY limY))
                    <*> (pure 0.0)
    vel3 :: (HasTime t s) => Wire s () IO a Vec3
    vel3 = (,,) <$> (snd <$> (axis kpX limX))
                    <*> (snd <$> (axis kpY limY))
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

renderPlayer :: Size -> Player -> IO ()
renderPlayer _ (Player ((x, y, _), _)) = do
  matrixMode $= Projection
  loadIdentity
  boxdepth >>= multMatrix
  boxshift x y >>= multMatrix
  color4d (1, 1, 1, 1)
  renderBox

renderHUD :: Size -> Player -> IO ()
renderHUD _ (Player ((x, y, _), _)) = do
  color4d (0, 0, 0, 1)
  renderXHair x y
