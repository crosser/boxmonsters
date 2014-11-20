-- Identified Flying Objects

module IFOs ( Vec3
            , LocVel
            , Launch
            , Steer(..)
            , Inputs(..)
            , XYZ(..)
            , steer
            , nsize
            , integralWith') where

import Control.Wire hiding ((.))
--import FRP.Netwire

type Vec3 = (Double, Double, Double)    -- (x, y, z) components
type LocVel = (Vec3, Vec3)              -- (location, velocity)
type Launch = Event LocVel

data Steer = Decr | Stay | Incr deriving Eq
data Inputs = Inputs { steerXY       :: (Steer, Steer)
                     , firePressed   :: Bool
                     , escapePressed :: Bool
                     , normSize      :: (Double, Double)
                     }

data XYZ = X | Y | Z

steer :: XYZ -> Inputs -> Steer
steer X = fst . steerXY
steer Y = snd . steerXY
steer Z = error "no steering over Z axis"

nsize :: XYZ -> Inputs -> Double
nsize X = fst . normSize
nsize Y = snd . normSize
nsize Z = error "Z size not normalizable"

integralWith' ::
    (Fractional a, HasTime t s)
    => (w -> a -> (a, b))  -- ^ Correction function.
    -> a                   -- ^ Integration constant (aka start value).
    -> Wire s e m (a, w) (a, b)
integralWith' correct = loop
    where
    loop x' =
        mkPure $ \ds (dx, w) ->
            let dt = realToFrac (dtime ds)
                (x, y)  = correct w (x' + dt*dx)
            in x' `seq` (Right (x', y), loop x)
