{-# LANGUAGE Arrows #-}

module World where

import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import IFOs
import Player
import Monster
import Projectile

data World = World Player [Monster] [Projectile]

worldWire :: (HasTime t s) => Wire s () IO (Double, Double) World
-- For now, just compose all wires.
-- Must also create monsters and handle collisions.
worldWire = proc size -> do
  player@(Player locvel launch) <- playerWire -< size
  monsters <- monstersWire -< ()
  projectiles <- projectilesWire -< ()
  returnA -< World player monsters projectiles

renderWorld :: (Double, Double) -> World -> IO ()
renderWorld size (World player monsters projectiles) = do
  clear [ColorBuffer, DepthBuffer]
  renderPlayer size player
  mapM_ (renderMonster size) monsters
  mapM_ (renderProjectile size) projectiles
  renderHUD size player
  swapBuffers
