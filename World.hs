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

-- to me moved to their respective modules
data Projectile = Projectile LocVel
projectilesWire :: Wire s () IO a [Projectile]
projectilesWire = (:[]) <$> (pure $ Projectile ((0,0,0),(0,0,0)))
-- end of to me moved to their respective modules

data World = World Player [Monster] [Projectile]

worldWire :: (HasTime t s) => Wire s () IO a World
-- For now, just compose all wires.
-- Must also create monsters and handle collisions.
worldWire = proc _ -> do
  player@(Player locvel launch) <- playerWire -< ()
  monsters <- monstersWire -< ()
  projectiles <- projectilesWire -< ()
  returnA -< World player monsters projectiles

renderWorld :: Size -> World -> IO ()
renderWorld size (World player monsters projectiles) = do
  clear [ColorBuffer, DepthBuffer]
  renderPlayer size player
  mapM_ (renderMonster size) monsters
  -- mapM_ renderProjectile size projectiles
  renderHUD size player
  swapBuffers
