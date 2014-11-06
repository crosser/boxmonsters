-- Identified Flying Objects

module IFOs ( FOVector
        , FOPosition
        , FOVelocity
        , FORender
        , FONature(..)
        , IFO(..)
        , foIsA) where

type FOVector = (Double, Double, Double)

type FOPosition = FOVector

type FOVelocity = FOVector

type FORender = IO ()

data FONature = Player | Projectile | GreenMonster | RedMonster
  deriving (Show, Eq)

data IFO = IFO  { render :: FORender
                , pos    :: FOPosition
                , vel    :: FOVelocity
                , nature :: FONature
                }

foIsA :: FONature -> IFO -> Bool
foIsA what who = nature who == what
