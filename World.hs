{-# LANGUAGE Arrows #-}

module World (World(..), worldWire, renderWorld) where

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

groupWires :: (Monad m, Monoid s)
           => Wire s e m (a, [Wire s e m a b]) [(b, Wire s e m a b)]
groupWires = mkGen $ \session (input, wires) -> do
  stepped <- mapM (\w -> stepWire w session (Right input)) wires
  return (Right [(out, wire) | (Right out, wire) <- stepped], groupWires)

worldWire :: (HasTime t s, MonadFix m) => Wire s () m Inputs World
-- For now, just compose all wires.
-- Must also create monsters and handle collisions.
worldWire = run . unless escapePressed
  where
    run = proc inp -> do
      player@(Player locvel plaunch) <- playerWire -< inp
      rec
        (ms, mws) <- unzip ^<< groupWires <<< second (delay imws) -< (inp, mws)
        (ps, pws) <- unzip ^<< groupWires <<< second (delay ipws) -< (inp, pws)
      returnA -< World player ms ps
    imws = [monsterWire (V3 0 0 1, V3 0 0 0)
           ,monsterWire (V3 0.5 0.5 1, V3 0.2 0.1 0)]
    ipws = [projectileWire (V3 0 0 0.5, V3 0 0 0.2)
           ,projectileWire (V3 (-0.5) (-0.5) 0.5, V3 0 0 0.1)]

renderWorld :: V3 Double -> World -> IO ()
renderWorld size (World player monsters projectiles) = do
  renderPlayer size player
  mapM_ (renderMonster size) monsters
  mapM_ (renderProjectile size) projectiles
  renderHUD size player
