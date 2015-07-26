{-# LANGUAGE Arrows #-}

module Projectile (Projectile(..), projectileWire, renderProjectile) where

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

projectileWire :: (HasTime t s, Monoid e, MonadFix m)
             => LocVel
             -> Wire s e m Inputs Projectile
projectileWire ilv = Projectile <$> bouncylv 0.0 ilv

-- Rendering projectile.

-- FIXME render shafts in the process of being reflected
renderShaft :: Loc -> Bool -> IO ()
renderShaft (V3 x y z) outgoing = do
  if outgoing then
    color4d (0, 1, 1, 1)
  else
    color4d (1, 0, 1, 1)
  renderPrimitive Lines $ mapM_ vertex3d 
    [ (x, y, z), (x, y, z-(if outgoing then hv; else (-hv))) ]

renderProjectile :: V3 Double -> Projectile -> IO ()
renderProjectile _ (Projectile (loc, (V3 _ _ vz))) = do
  -- matrixMode $= Modelview 0
  -- loadIdentity
  renderShaft loc (vz > 0)
