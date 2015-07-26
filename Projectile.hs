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

bottom = 1.0

data Projectile = Projectile LocVel

-- | Projectile velocity - keep original until bounce
-- Input signal is whether it has bounced off the wall.

velocity :: (Monoid e, MonadFix m)
         => Vel -> Wire s e m (V3 Bounce) Vel
velocity iv = v3wi iv vel1
  where
  vel1 :: (Monoid e, MonadFix m) => Double -> Wire s e m Bounce Double
  vel1 iv = mkSFN $ \bounce -> let v = fixv iv bounce in (v, vel1 v)
  fixv :: Double -> Bounce -> Double
  fixv iv bounce= case bounce of
    MkNeg -> - abs iv
    MkPos ->   abs iv
    Keep  ->   iv

location :: (HasTime t s, Monoid e, MonadFix m)
         => Loc
         -> Wire s e m Vel (Loc, V3 Bounce)
location il = checkbounce . integral il
  where
    checkbounce :: Wire s e m Loc (Loc, V3 Bounce)
    checkbounce = mkSF_ $ \loc@(V3 x y z) ->
      if z > bottom -- Only bounce off the bootom for now
        then (V3 x y (2*bottom - z), V3 Keep Keep MkNeg)
        else (loc, V3 Keep Keep Keep)

locvel :: (HasTime t s, Monoid e, MonadFix m)
     => LocVel
     -> Wire s e m a LocVel
locvel (il, iv) = proc _ -> do
  rec (loc, bounce) <- location il -< vel
      vel <- velocity iv -< bounce
  returnA -< (loc, vel)

projectileWire :: (HasTime t s, Monoid e, MonadFix m)
             => LocVel
             -> Wire s e m a Projectile
projectileWire ilv = Projectile <$> locvel ilv

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
