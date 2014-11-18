-- Identified Flying Objects

module IFOs ( Vec3
            , LocVel
            , Launch
            , Steer(..)
            , Inputs(..)
            , XorY(..)
            , steer
            , nsize) where

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

data XorY = X | Y

steer :: XorY -> Inputs -> Steer
steer X = fst . steerXY
steer Y = snd . steerXY

nsize :: XorY -> Inputs -> Double
nsize X = fst . normSize
nsize Y = snd . normSize
