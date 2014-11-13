-- Identified Flying Objects

module IFOs (Vec3, PosVel) where

type Vec3 = (Double, Double, Double)    -- (x, y, z) components
type PosVel = (Vec3, Vec3)              -- (position, velocity)
