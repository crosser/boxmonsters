-- Identified Flying Objects

module IFOs (Vec3, LocVel, Launch) where

import Control.Wire

type Vec3 = (Double, Double, Double)    -- (x, y, z) components
type LocVel = (Vec3, Vec3)              -- (location, velocity)
type Launch = Event LocVel
