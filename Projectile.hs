{-# LANGUAGE Arrows #-}

module Projectile (Projectile(..), projectilesWire, renderProjectile) where

import Prelude hiding ((.), id)
import Control.Monad.Fix
import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import GLUtils
import IFOs

hv = 0.1

-- Functional part.

data Projectile = Projectile LocVel

projectilesWire :: (MonadFix m)
                => [Projectile]
                -> Wire s () m Launch [Projectile]
{-
projectilesWire ps = mkSFN $ \locvel ->
  ps `seq` (ps, projectilesWire ((mkProjectile locvel):ps))

mkProjectile :: (MonadFix m)
             => LocVel
             -> Wire s () m a Projectile
mkProjectile locvel = pure $ Projectile ((0,0,0.5),(0,0,0.1))
-}
projectilesWire _ = (:[]) <$> (pure $ Projectile ((0,0,0.5),(0,0,0.1)))

-- Rendering projectile.

-- FIXME render shafts in the process of being reflected
renderShaft :: Vec3 -> Bool -> IO ()
renderShaft (x, y, z) outgoing = do
  if outgoing then
    color4d (0, 1, 1, 1)
  else
    color4d (1, 0, 1, 1)
  renderPrimitive Lines $ mapM_ vertex3d 
    [ (x, y, z), (x, y, z-(if outgoing then hv; else (-hv))) ]

renderProjectile :: (Double, Double) -> Projectile -> IO ()
renderProjectile _ (Projectile (loc, (_, _, vz))) = do
  -- matrixMode $= Modelview 0
  -- loadIdentity
  renderShaft loc (vz > 0)
