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

zbottom = 1.0

data Projectile = Projectile LocVel

-- | Projectile velocity by axis x, y or z.
-- Input signal is whether it has hit the bottom.

velocity :: (MonadFix m, Monoid e)
         => Double
         -> Wire s e m Bool Double
velocity iv = mkSFN $ \bounced ->
  iv `seq` (iv, velocity (if bounced then -iv else iv))

location :: (HasTime t s, MonadFix m)
         => XYZ
         -> Double
         -> Wire s () m Double (Double, Bool)
location xyz il = checkbounce xyz . integral il
  where
    checkbounce :: XYZ -> Wire s e m Double (Double, Bool)
    checkbounce xyz = mkSF_ $ \loc ->
      case xyz of
        Z -> if loc > zbottom then ((2*zbottom - loc), True) else (loc, False)
        _ -> (loc, False)

axis :: (HasTime t s, MonadFix m)
     => XYZ
     -> (Double, Double)
     -> Wire s () m a (Double, Double) -- (location, velocity) for axis
axis xyz (il, iv) = proc _ -> do
  rec (loc, bounce) <- location xyz il -< vel
      vel <- velocity iv -< bounce
  returnA -< (loc, vel)

mkProjectile :: (HasTime t s, MonadFix m)
             => LocVel
             -> Wire s () m a Projectile
mkProjectile ((ilx, ily, ilz), (ivx, ivy, ivz)) = Projectile <$> locvel
  where
    locvel = (,) <$> loc3 <*> vel3
    loc3 = (,,) <$> (fst <$> axis X (ilx, ivx))
                <*> (fst <$> axis Y (ily, ivy))
                <*> (fst <$> axis Z (ilz, ivz))
    vel3 = (,,) <$> (snd <$> axis X (ilx, ivx))
                <*> (snd <$> axis Y (ily, ivy))
                <*> (snd <$> axis Z (ilz, ivz))

projectilesWire :: (HasTime t s, MonadFix m)
                => [Projectile]
                -> Wire s () m Launch [Projectile]
{-
projectilesWire ps = mkSFN $ \locvel ->
  ps `seq` (ps, projectilesWire ((mkProjectile locvel):ps))
-}
projectilesWire _ = (:[]) <$> (pure $ Projectile ((0,0,0.5),(0,0,0.1)))
--projectilesWire _ = (:[]) <$> (pure $ Projectile ((0,0,0.5),(0,0,0.1)))

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
