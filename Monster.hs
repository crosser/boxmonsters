{-# LANGUAGE Arrows #-}

module Monster (Monster(..), monsterWire, renderMonster) where

import Prelude hiding ((.), id)
import Control.Monad.Fix
import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import GLUtils
import IFOs

data MonsterLevel = GreenMonster | RedMonster
data Monster = Monster LocVel MonsterLevel

-- Functional part.

hvsize = 0.05

-- | Monster velocity - keep original until bounce
-- Input signal is whether it has bounced off the wall.

velocity :: (Monoid e, MonadFix m)
         => Vel -> Wire s e m (V3 Bool) Vel
velocity iv = v3wi iv vel1
  where
  vel1 :: (Monoid e, MonadFix m) => Double -> Wire s e m Bool Double
  vel1 iv = mkSFN $ \bounce -> let v = fixv iv bounce in (v, vel1 v)
  fixv :: Double -> Bool -> Double
  fixv iv bounce= if bounce then -iv else iv

location :: (HasTime t s, Monoid e, MonadFix m)
         => Loc
         -> Wire s e m (V3 Double, Vel) (Loc, V3 Bool)
location il = proc (size, vel) -> do
  rawloc <- integral il -< vel
  bounced <- v2pair ^<< v3w (mkSF_ bounce1) -< pair2v (rawloc, size)
  returnA -< bounced
  where
    bounce1 (x, size)
      | x < lo    = (2*lo - x, True)
      | x > hi    = (2*hi - x, True)
      | otherwise = (x, False)
      where
        lo = hvsize - size
        hi = size - hvsize

locvel :: (HasTime t s, Monoid e, MonadFix m)
     => LocVel
     -> Wire s e m Inputs LocVel
locvel (il, iv) = proc inputs -> do
  rec (loc, bounce) <- location il -< ((norm inputs), vel)
      vel <- velocity iv -< bounce
  returnA -< (loc, vel)

monsterWire :: (HasTime t s, Monoid e, MonadFix m)
             => LocVel
             -> Wire s e m Inputs Monster
monsterWire ilv = Monster <$> locvel ilv <*> pure GreenMonster

-- Rendering Monster.

renderFace :: Loc -> IO ()
renderFace (V3 x y z) =
  renderPrimitive Quads $ mapM_ vertex3d
    [ (x-hvsize, y-hvsize, z)
    , (x+hvsize, y-hvsize, z)
    , (x+hvsize, y+hvsize, z)
    , (x-hvsize, y+hvsize, z)
    ]

renderMonster :: V3 Double -> Monster -> IO ()
renderMonster _ (Monster (loc, _) level) = do
  -- matrixMode $= Modelview 0
  -- loadIdentity
  case level of
    GreenMonster -> color4d (0, 1, 0, 1)
    RedMonster -> color4d (1, 0, 0, 1)
  renderFace loc
