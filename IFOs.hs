-- | Identified Flying Objects

{-# LANGUAGE Arrows #-}

module IFOs ( V3(V3)
            , Loc
            , Vel
            , LocVel
            , Steer(..)
            , Inputs(..)
            , v2pair
            , pair2v
            , v3w
            , v3wi
            , bouncylv
            ) where

import Prelude hiding ((.), id)
import Control.Monad.Fix
import Control.Wire
import FRP.Netwire

-- V3 type and instances borrowed from `linear` package

data V3 a = V3 a a a deriving Eq        -- (x, y, z) components

instance Show a => Show (V3 a) where
  show (V3 x y z) =
    "<" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ ">"

instance Functor V3 where
  fmap f (V3 x y z) = V3 (f x) (f y) (f z)
  v <$ _ = V3 v v v

instance Applicative V3 where
  pure v = V3 v v v
  V3 f g h <*> V3 x y z = V3 (f x) (g y) (h z)

-- FIXME: understand how the definition of (>>=) works
instance Monad V3 where
  return a = V3 a a a
  V3 a b c >>= f = V3 a' b' c' where
    V3 a' _ _ = f a
    V3 _ b' _ = f b
    V3 _ _ c' = f c

instance Num a => Num (V3 a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = pure . fromInteger

instance Fractional a => Fractional (V3 a) where
  recip = fmap recip
  (/) = liftA2 (/)
  fromRational = pure . fromRational

-- repackaging V3 (a, b) -> (V3 a, V3 b) and back

v2pair :: Functor f => f (a, b) -> (f a, f b)
v2pair uv = (fmap fst uv, fmap snd uv)

pair2v :: Applicative f => (f a, f b) -> f (a, b)
pair2v (u, v) = (,) <$> u <*> v

v3w :: Monad m => Wire s e m a b -> Wire s e m (V3 a) (V3 b)
v3w w = V3 <$> (w <<^ v3x) <*> (w <<^ v3y) <*> (w <<^ v3z)

v3wi :: Monad m => V3 i -> (i -> Wire s e m a b) -> Wire s e m (V3 a) (V3 b)
v3wi (V3 x y z) w = V3 <$> (w x <<^ v3x) <*> (w y <<^ v3y) <*> (w z <<^ v3z)

v3x (V3 x _ _) = x
v3y (V3 _ y _) = y
v3z (V3 _ _ z) = z

-- IFO characteristics and controls

type Loc = V3 Double
type Vel = V3 Double
type LocVel = (Loc, Vel)

data Steer = Decr | Stay | Incr deriving Eq
data Inputs = Inputs { steer         :: V3 Steer
                     , firePressed   :: Bool
                     , escapePressed :: Bool
                     , norm          :: V3 Double
                     }

-- | Bouncy object velocity - keep original until bounce
-- Input signal is whether it has bounced off the wall.

velocity :: (Monoid e, MonadFix m)
         => Vel -> Wire s e m (V3 Bool) Vel
velocity iv = v3wi iv vel1
  where
  vel1 :: (Monoid e, MonadFix m) => Double -> Wire s e m Bool Double
  vel1 iv = mkSFN $ \bounce -> let v = fixv iv bounce in (v, vel1 v)
  fixv :: Double -> Bool -> Double
  fixv iv bounce= if bounce then -iv else iv

-- | Bouncy object location - inegrate velocity until bounce
-- Input signal is limits and velocity

location :: (HasTime t s, Monoid e, MonadFix m)
         => Double
         -> Loc
         -> Wire s e m (V3 Double, Vel) (Loc, V3 Bool)
location hvsize il = proc (size, vel) -> do
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

bouncylv :: (HasTime t s, Monoid e, MonadFix m)
         => Double
         -> LocVel
         -> Wire s e m Inputs LocVel
bouncylv hvsize (il, iv) = proc inputs -> do
  rec (loc, bounce) <- location hvsize il -< ((norm inputs), vel)
      vel <- velocity iv -< bounce
  returnA -< (loc, vel)

