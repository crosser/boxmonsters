-- Identified Flying Objects

module IFOs (Vec3, LocVel, Launch, Steer(..), Inputs(..)) where

import Control.Wire
import FRP.Netwire

type Vec3 = (Double, Double, Double)    -- (x, y, z) components
type LocVel = (Vec3, Vec3)              -- (location, velocity)
type Launch = Event LocVel

data Steer = Decr | Stay | Incr deriving Eq
data Inputs = Inputs { steerX        :: Steer
                     , steerY        :: Steer
                     , firePressed   :: Bool
                     , escapePressed :: Bool
                     , normSize      :: (Double, Double)
                     }
