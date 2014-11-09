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

playerVelX :: (Monoid e) => Wire s e IO a Double
playerVelX = pure ( 0.0) . isKeyDown LEFT . isKeyDown RIGHT
         <|> pure (-0.5) . isKeyDown LEFT
         <|> pure ( 0.5) . isKeyDown RIGHT
         <|> pure ( 0.0)

playerVelY :: (Monoid e) => Wire s e IO a Double
playerVelY = pure ( 0.0) . isKeyDown DOWN . isKeyDown UP
         <|> pure (-0.5) . isKeyDown DOWN
         <|> pure ( 0.5) . isKeyDown UP
         <|> pure ( 0.0)

playerVel :: (Monoid e) => Wire s e IO a FOVelocity
playerVel = FOVector <$> playerVelX <*> playerVelY <*> (pure 0.0)

clamp :: (Double, Double) -> Wire s e m Double (Double, Bool)
clamp lim = mkPure_ $ clamp' lim
  where
    clamp' :: (Double, Double) -> Double -> Either e (Double, Bool)
    clamp' (lo, hi) x
      | x < lo    = Right (lo, True)
      | x > hi    = Right (hi, True)
      | otherwise = Right (x,  False)

playerPosX :: (HasTime t s) => Wire s () IO a Double
playerPosX = integral 0 . playerVelX

playerPosY :: (HasTime t s) => Wire s () IO a Double
playerPosY = integral 0 . playerVelY

playerPos :: (HasTime t s) => Wire s () IO a FOPosition
playerPos = FOVector <$> playerPosX <*> playerPosY <*> (pure 0.0)

playerWire :: (HasTime t s) => Wire s () IO a IFO
playerWire = IFO <$> (renderPlayer <$> playerPos)
                 <*> playerPos
                 <*> playerVel
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

