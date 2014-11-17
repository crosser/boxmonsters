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

import IFOs
import World

initialWSize = Size 640 480

-- | Normalize such that shorter dimetion is 1., longer is >1.
normWSize :: Size -> (Double, Double)
normWSize (Size x y)
  | x > y = (fromIntegral x / fromIntegral y, 1)
  | otherwise = (1, fromIntegral y / fromIntegral x)

runWire :: (HasTime t s)
  => (IORef Bool, IORef (Double, Double))
  -> Session IO s -> Wire s e IO (Double, Double) World
  -> IO ()
runWire (closedRef, sizeRef) session wire = do
  pollEvents
  closed <- readIORef closedRef
  size <- readIORef sizeRef
  esc <- getKey ESC
  if closed || (esc == Press)
    then return ()
    else do
      (st , session') <- stepSession session
      (wt', wire'   ) <- stepWire wire st $ Right size
      case wt' of
        Left  _ -> return ()
        Right worldstate -> do
          renderWorld size worldstate
          runWire (closedRef, sizeRef) session' wire'

main :: IO ()
main = do
  initialize
  openWindow initialWSize
                        [ DisplayRGBBits 8 8 8
                        , DisplayAlphaBits 8
                        , DisplayDepthBits 24
                        ] Window
  closedRef <- newIORef False
  sizeRef <- newIORef (normWSize initialWSize)
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
  windowSizeCallback $= \ size ->
    do
      writeIORef sizeRef (normWSize size)
      viewport $= (Position 0 0, size)

  runWire (closedRef, sizeRef) clockSession_ worldWire

  closeWindow
  terminate
