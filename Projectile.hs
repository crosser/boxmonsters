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

bottom = 1.0

data Projectile = Projectile LocVel

-- | Projectile velocity by axis x, y or z.
-- Input signal is whether it has hit the bottom.

velocity :: (MonadFix m, Monoid e)
         => Double
         -> Wire s e m Bounce Double
velocity iv = mkSFN $ \bounce -> (iv, velocity (fixv iv bounce))
  where
    fixv iv bounce= case bounce of
      MkNeg -> - abs iv
      MkPos ->   abs iv
      Keep  ->   iv

location :: (HasTime t s, MonadFix m)
         => XYZ
         -> Double
         -> Wire s () m Double (Double, Bounce)
location xyz il = checkbounce xyz . integral il
  where
    checkbounce :: XYZ -> Wire s e m Double (Double, Bounce)
    checkbounce xyz = mkSF_ $ \loc ->
      case xyz of
        Z -> if loc > bottom then ((2*bottom - loc), MkNeg) else (loc, Keep)
        _ -> (loc, Keep)

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

mergeLaunches :: (HasTime t s, MonadFix m)
                => [Wire s () m a Projectile]
                -> Wire s () m LocVel [Wire s () m a Projectile]
mergeLaunches ps = mkSFN $ \locvel ->
  ps `seq` (ps, mergeLaunches ((mkProjectile locvel):ps))

foldProjectiles :: (HasTime t s, MonadFix m)
                => Wire s () m [Wire s () m a Projectile] [Projectile]
foldProjectiles = pure []

projectilesWire :: (HasTime t s, MonadFix m)
                => Wire s () m Launch [Projectile]
{-
projectilesWire = proc launch -> do
  rec
    pws' <- mergeLaunches -< (pws, launch)
    ps' <- foldProjectiles -< pws'
    (pws, ps) <- filtProjectiles -< (pws', ps')
  returnA -< ps
-}
-- projectilesWire = foldProjectiles . (mergeLaunches [] . hold <|> pure [])
-- FIXME
--projectilesWire = (:[]) <$> (pure $ Projectile ((0, 0, 0.5), (0, 0, 0.1)))
projectilesWire = (:[]) <$> (mkProjectile ((0, 0, 0.5), (0, 0, 0.1)))

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
