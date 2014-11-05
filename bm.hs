{-
        Box Monsters

        Copyright 2014 Eugene Crosser

        Remake of Flying Arrows from 1990
        which in turn was an an effort to make Space Invaders
        (née Space Monsters) a first person 3D experience.
-}

import Prelude hiding ((.), id)
import Control.Monad.IO.Class

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Data.IORef

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

renderWorld :: Size -> (Double, Double) -> IO ()
renderWorld size@(Size w h) (x, y) = do
  loadIdentity
  mydepth >>= multMatrix
  myshift x y >>= multMatrix
  clear [ColorBuffer, DepthBuffer]
  renderColor (1, 1, 1, 1)
  renderBox
  renderColor (0, 0, 0, 1)
  renderPrimitive Points $
    renderPoint (x, y, 0.5)
  swapBuffers

runWire :: (HasTime t s) =>
  (IORef Bool, IORef Size) -> Session IO s -> Wire s e IO a (Double, Double) -> IO ()
runWire (closedRef, sizeRef) session wire = do
  pollEvents
  closed <- readIORef closedRef
  size <- readIORef sizeRef
  esc <- getKey ESC
  if closed || (esc == Press)
    then return ()
    else do
      (st , session') <- stepSession session
      (wt', wire'   ) <- stepWire wire st $ Right undefined
      case wt' of
        Left  _ -> return ()
        Right worldstate -> do
          renderWorld size worldstate
          runWire (closedRef, sizeRef) session' wire'

main :: IO ()
main = do
  initialize
  openWindow (Size 640 480)
                        [ DisplayRGBBits 8 8 8
                        , DisplayAlphaBits 8
                        , DisplayDepthBits 24
                        ] Window
  closedRef <- newIORef False
  sizeRef <- newIORef (Size 640 480)
  windowCloseCallback $= do
    writeIORef closedRef True
    return True
  windowTitle $= "Box Monsters"
  -- depthFunc $= Just Less
  shadeModel $= Smooth
  lineSmooth $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  lineWidth $= 1.5
  pointSize $= 5
  clearColor $= Color4 0 0 1 0
  windowSizeCallback $= \ size@(Size w h) ->
    do
      writeIORef sizeRef size
      viewport $= (Position 0 0, size)

  runWire (closedRef, sizeRef) clockSession_ podPos

  closeWindow
  terminate
