{-# LANGUAGE Arrows #-}

module Projectile (Projectile(..), projectilesWire, renderProjectile) where

import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import GLUtils
import IFOs

hv = 0.1

-- Functional part.

data Projectile = Projectile LocVel
projectilesWire :: Wire s () IO a [Projectile]
projectilesWire = (:[]) <$> (pure $ Projectile ((0,0,0.5),(0,0,0.1)))

-- Rendering projectile.

renderShaft :: Vec3 -> IO ()
renderShaft (x, y, z) =
  renderPrimitive Lines $ mapM_ vertex3d
  [ (x, y, z), (x, y, z-hv) ]

renderProjectile :: Size -> Projectile -> IO ()
renderProjectile _ (Projectile (loc, (_, _, vz))) = do
  -- matrixMode $= Modelview 0
  -- loadIdentity
  if vz > 0 then
    color4d (0, 1, 1, 1)
  else
    color4d (1, 0, 1, 1)
  renderShaft loc
