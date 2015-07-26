{-
        Box Monsters

        Copyright 2014 Eugene Crosser

        Remake of Flying Arrows from 1990
        which in turn was an an effort to make Space Invaders
        (née Space Monsters) a first person 3D experience.
-}

import Prelude hiding ((.), id)
import Control.Wire
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Data.IORef

import IFOs
import World

-- | Window (and viewport) size for the playing field.

initialWSize = Size 640 480

-- | Normalize such that shorter dimetion is 1., longer is >1.

normWSize :: Size -> V3 Double
normWSize (Size x y)
  | x > y     = V3 (fromIntegral x / fromIntegral y) 1 1
  | otherwise = V3 1 (fromIntegral y / fromIntegral x) 1

-- | Run the outermost Wire giving it all the external inputs as signal

runWire :: (HasTime t s)
  => (IORef Bool, IORef (V3 Double))
  -> Session IO s -> Wire s e IO Inputs World
  -> IO ()
runWire (closedRef, sizeRef) session wire = do
  -- pollEvents -- not necessary if we do swapBuffers
  closed <- readIORef closedRef
  nsize  <- readIORef sizeRef
  esc    <- getKey ESC
  up     <- getKey UP
  down   <- getKey DOWN
  left   <- getKey LEFT
  right  <- getKey RIGHT
  enter  <- getKey ENTER
  space  <- getKey ' '
  let
    steer k1 k2 = case (k1, k2) of
      (Press,   Press)   -> Stay
      (Press,   Release) -> Decr
      (Release, Press)   -> Incr
      (Release, Release) -> Stay
    inputs = Inputs { steer         = V3 (steer left right) (steer down up) Stay
                    , firePressed   = (enter == Press) || (space == Press)
                    , escapePressed = (esc  == Press)
                    , norm          = nsize
                    }
  if closed then return () else do
    (st , session') <- stepSession session
    (wt', wire'   ) <- stepWire wire st $ Right inputs
    case wt' of
      Left  _ -> return ()
      Right worldstate -> do
        clear [ColorBuffer, DepthBuffer]
        renderWorld nsize worldstate
        swapBuffers
        runWire (closedRef, sizeRef) session' wire'

main :: IO ()
main = do
  initialize
  openWindow initialWSize [ DisplayRGBBits 8 8 8
                          , DisplayAlphaBits 8
                          , DisplayDepthBits 24
                          ] Window
  closedRef <- newIORef False
  sizeRef <- newIORef (normWSize initialWSize)
  windowTitle $= "Box Monsters"
  -- depthFunc $= Just Less
  shadeModel $= Smooth
  lineSmooth $= Enabled
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  pointSize $= 5
  clearColor $= Color4 0 0 1 0
  windowCloseCallback $= do
    writeIORef closedRef True
    return True
  windowSizeCallback $= \ size -> do
    writeIORef sizeRef (normWSize size)
    viewport $= (Position 0 0, size)

  runWire (closedRef, sizeRef) clockSession_ worldWire

  closeWindow
  terminate
