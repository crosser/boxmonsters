-- Identified Flying Objects

module IFOs ( Vec3(..)
        , FORender
        , FONature(..)
        , IFO(..)
        , foIsA) where

type Vec3 = (Double, Double, Double) -- (x, y, z) components

type FORender = IO ()

data FONature = Player | Projectile | GreenMonster | RedMonster
  deriving (Show, Eq)

data IFO = IFO  { render :: FORender
                , pos    :: Vec3
                , vel    :: Vec3
                , nature :: FONature
                }

foIsA :: FONature -> IFO -> Bool
foIsA what who = nature who == what
