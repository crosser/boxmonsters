module Player (renderPlayer, playerWire) where

import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import IFOs

podSpeedX :: (Monoid e) => Wire s e IO a Double
podSpeedX = pure ( 0.0) . isKeyDown LEFT . isKeyDown RIGHT
        <|> pure (-0.5) . isKeyDown LEFT
        <|> pure ( 0.5) . isKeyDown RIGHT
        <|> pure ( 0.0)

podSpeedY :: (Monoid e) => Wire s e IO a Double
podSpeedY = pure ( 0.0) . isKeyDown DOWN . isKeyDown UP
        <|> pure (-0.5) . isKeyDown DOWN
        <|> pure ( 0.5) . isKeyDown UP
        <|> pure ( 0.0)

podPos :: (HasTime t s) => Wire s () IO a (Double, Double)
podPos = (integral 0 . podSpeedX) &&& (integral 0 . podSpeedY)

playerWire :: (HasTime t s) => Wire s () IO a IFO
playerWire = pure $ IFO { render = renderPlayer (0, 0, 0)
                        , pos    = (0, 0, 0)
                        , vel    = (0, 0, 0)
                        , nature = Player
                        }

-------------------------

isKeyDown :: (Enum k, Monoid e) => k -> Wire s e IO a e
isKeyDown k = mkGen_ $ \_ -> do
  s <- getKey k
  return $ case s of
    Press   -> Right mempty
    Release -> Left  mempty

renderColor :: (Double, Double, Double, Double) -> IO ()
renderColor (r, g, b, a) =
  color $ Color4 (realToFrac r :: GLfloat)
                 (realToFrac g :: GLfloat)
                 (realToFrac b :: GLfloat)
                 (realToFrac a :: GLfloat)

renderPoint :: (Double, Double, Double) -> IO ()
renderPoint (x, y, z) =
  vertex $ Vertex3 (realToFrac x :: GLfloat)
                   (realToFrac y :: GLfloat)
                   (realToFrac z :: GLfloat)

renderBox :: IO ()
renderBox =
  mapM_ (renderPrimitive LineStrip . mapM_ renderPoint)
    [ [((-1), (-1), 0), ((-1), (-1), 1), ((-1),   1 , 1)]
    , [((-1),   1 , 0), ((-1),   1 , 1), (  1 ,   1 , 1)]
    , [(  1 ,   1 , 0), (  1 ,   1 , 1), (  1 , (-1), 1)]
    , [(  1 , (-1), 0), (  1 , (-1), 1), ((-1), (-1), 1)]
    ]

renderXHair :: Double -> Double -> IO ()
renderXHair x y =
  mapM_ (renderPrimitive Lines . mapM_ renderPoint)
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
  renderColor (1, 1, 1, 1)
  renderBox
  renderColor (0, 0, 0, 1)
  renderXHair x y
