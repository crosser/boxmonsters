{-# LANGUAGE Arrows #-}

module World where

import Prelude hiding ((.), id)
import Control.Monad.Fix
import Control.Wire
--import FRP.Netwire -- will likely need it for collisions
import Graphics.Rendering.OpenGL

import IFOs
import Player
import Monster
import Projectile

data World = World Player [Monster] [Projectile]

worldWire :: (HasTime t s, MonadFix m) => Wire s () m Inputs World
-- For now, just compose all wires.
-- Must also create monsters and handle collisions.
worldWire = run . unless escapePressed
  where
    run = proc inputs -> do
      player@(Player locvel plaunch) <- playerWire -< inputs
      rec
        monsters <- monstersWire -< ()
        projectiles <- projectilesWire -< plaunch
      returnA -< World player monsters projectiles

renderWorld :: (Double, Double) -> World -> IO ()
renderWorld size (World player monsters projectiles) = do
  renderPlayer size player
  mapM_ (renderMonster size) monsters
  mapM_ (renderProjectile size) projectiles
  renderHUD size player
