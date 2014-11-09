{-# LANGUAGE Arrows #-}

module Player (renderPlayer, playerWire) where

import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import GLUtils
import IFOs

-- Functional part. Player is a wire controlled by keypresses.

isKeyDown :: (Enum k, Monoid e) => k -> Wire s e IO a e
isKeyDown k = mkGen_ $ \_ -> do
  s <- getKey k
  return $ case s of
    Press   -> Right mempty
    Release -> Left  mempty

propulsionX :: (Monoid e) => Wire s e IO a Double
propulsionX = pure ( 0.0) . isKeyDown LEFT . isKeyDown RIGHT
         <|> pure (-0.5) . isKeyDown LEFT
         <|> pure ( 0.5) . isKeyDown RIGHT
         <|> pure ( 0.0)

propulsionY :: (Monoid e) => Wire s e IO a Double
propulsionY = pure ( 0.0) . isKeyDown DOWN . isKeyDown UP
         <|> pure (-0.5) . isKeyDown DOWN
         <|> pure ( 0.5) . isKeyDown UP
         <|> pure ( 0.0)

-- Produce if input signal is True otherwise inhibit
isstuck :: (Monoid e) => Wire s e m Bool e
isstuck = mkPure_ $ \hit -> if hit then Right mempty else Left mempty

playerVelX :: (Monoid e) => Wire s e IO Bool Double
playerVelX = pure ( 0.0) . isstuck <|> propulsionX

playerVelY :: (Monoid e) => Wire s e IO Bool Double
playerVelY = pure ( 0.0) . isstuck <|> propulsionY

propulsion :: (Monoid e) => Wire s e IO a FOVelocity
propulsion = FOVector <$> propulsionX <*> propulsionY <*> (pure 0.0)

clamp :: (Double, Double) -> Wire s e m Double (Double, Bool)
clamp lim = mkPure_ $ clamp' lim
  where
    clamp' :: (Double, Double) -> Double -> Either e (Double, Bool)
    clamp' (lo, hi) x
      | x < lo    = Right (lo, True)
      | x > hi    = Right (hi, True)
      | otherwise = Right (x,  False)

positionW :: (HasTime t s) => Wire s () IO Double (Double, Bool)
positionW = clamp ((-1), 1) . integral 0

playerPosX :: (HasTime t s) => Wire s () IO a Double
playerPosX = proc _ -> do
  rec (pos, coll) <- positionW -< vel
      vel <- playerVelX -< coll
  returnA -< pos

playerPosY :: (HasTime t s) => Wire s () IO a Double
playerPosY = proc _ -> do
  rec (pos, coll) <- positionW -< vel
      vel <- playerVelY -< coll
  returnA -< pos

playerPos :: (HasTime t s) => Wire s () IO a FOPosition
playerPos = FOVector <$> playerPosX <*> playerPosY <*> (pure 0.0)

playerWire :: (HasTime t s) => Wire s () IO a IFO
playerWire = IFO <$> (renderPlayer <$> playerPos)
                 <*> playerPos
                 <*> propulsion
                 <*> (pure Player)

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

renderPlayer :: FOPosition -> IO ()
renderPlayer (FOVector x y _) = do
  matrixMode $= Projection
  loadIdentity
  boxdepth >>= multMatrix
  boxshift x y >>= multMatrix
  color4d (1, 1, 1, 1)
  renderBox
  color4d (0, 0, 0, 1)
  renderXHair x y

