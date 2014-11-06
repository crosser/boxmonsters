module Player (renderPlayer, playerWire) where

import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import GLUtils
import IFOs

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

mydepth :: IO (GLmatrix GLfloat)
mydepth = newMatrix RowMajor [ 1, 0, 0, 0
                             , 0, 1, 0, 0
                             , 0, 0, 0, 0
                             , 0, 0, 2, 0
                             ]

myshift :: Double -> Double -> IO (GLmatrix GLfloat)
myshift x y = newMatrix RowMajor [ 1, 0, 0, -(realToFrac x :: GLfloat)
                                , 0, 1, 0, -(realToFrac y :: GLfloat)
                                , 0, 0, 1, 0
                                , 0, 0, 0, 1
                                ]

renderPlayer :: FOPosition -> IO ()
renderPlayer (x, y, _) = do
  matrixMode $= Projection
  loadIdentity
  mydepth >>= multMatrix
  myshift x y >>= multMatrix
  color4d (1, 1, 1, 1)
  renderBox
  color4d (0, 0, 0, 1)
  renderXHair x y

-- Functional part. Player is a wire controlled by keypresses.

isKeyDown :: (Enum k, Monoid e) => k -> Wire s e IO a e
isKeyDown k = mkGen_ $ \_ -> do
  s <- getKey k
  return $ case s of
    Press   -> Right mempty
    Release -> Left  mempty

playerSpeedX :: (Monoid e) => Wire s e IO a Double
playerSpeedX = pure ( 0.0) . isKeyDown LEFT . isKeyDown RIGHT
           <|> pure (-0.5) . isKeyDown LEFT
           <|> pure ( 0.5) . isKeyDown RIGHT
           <|> pure ( 0.0)

playerSpeedY :: (Monoid e) => Wire s e IO a Double
playerSpeedY = pure ( 0.0) . isKeyDown DOWN . isKeyDown UP
           <|> pure (-0.5) . isKeyDown DOWN
           <|> pure ( 0.5) . isKeyDown UP
           <|> pure ( 0.0)

playerPos :: (HasTime t s) => Wire s () IO a (Double, Double)
playerPos = (integral 0 . playerSpeedX) &&& (integral 0 . playerSpeedY)

playerWire :: (HasTime t s) => Wire s () IO a IFO
playerWire = pure $ IFO { render = renderPlayer (0, 0, 0)
                        , pos    = (0, 0, 0)
                        , vel    = (0, 0, 0)
                        , nature = Player
                        }

-------------------------
