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

runWire :: (HasTime t s) =>
  (IORef Bool, IORef Size) -> Session IO s -> Wire s e IO a World -> IO ()
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

  runWire (closedRef, sizeRef) clockSession_ worldWire

  closeWindow
  terminate
