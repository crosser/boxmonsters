module World where

import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import IFOs
import Player

type World = IFO -- FIXME - should be [IFO]

renderWorld :: Size -> World -> IO ()
renderWorld size ws = do
  clear [ColorBuffer, DepthBuffer]
  render ws
  --mapM_ render $ filter (foIsA Player) ws
  --mapM_ render $ filter (not.foIsA Player) ws
  swapBuffers

worldWire :: (HasTime t s) => Wire s () IO a World
worldWire = playerWire -- FIXME - how to combine/decompose list of wires?
