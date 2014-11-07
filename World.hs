module World where

import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import IFOs
import Player

type World = [IFO]

renderWorld :: Size -> World -> IO ()
renderWorld size ws = do
  clear [ColorBuffer, DepthBuffer]
  mapM_ render $ filter (foIsA Player) ws
  mapM_ render $ filter (not.foIsA Player) ws
  swapBuffers

worldWire :: (HasTime t s) => Wire s () IO a World
-- for now, make it of just a single Player element
worldWire = (:[]) <$> playerWire
