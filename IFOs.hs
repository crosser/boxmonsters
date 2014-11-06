-- Identified Flying Objects

module IFOs where

type Vector = (Double, Double, Double)

type Position = Vector

type Velocity = Vector

type Render = IO ()

data IFONature = Player | Projectile | GreenMonster | RedMonster
  deriving (Show, Eq)

data IFO = IFO  { render :: Render
                , pos :: Position
                , vel :: Velocity
                , nature :: IFONature
                }

isA :: IFONature -> IFO -> Bool
isA what who = nature who == what

type World = [IFO]

mkPlayer :: Position -> Velocity -> IFO
mkPlayer p v =
  IFO { render = putStrLn $ "P pos=" ++ (show p) ++ ", vel=" ++ (show v)
      , pos = p
      , vel = v
      , nature = Player
      }

mkGreenMonster :: Position -> Velocity -> IFO
mkGreenMonster p v =
  IFO { render = putStrLn $ "G pos=" ++ (show p) ++ ", vel=" ++ (show v)
      , pos = p
      , vel = v
      , nature = GreenMonster
      }

p = mkPlayer (1,1,1) (2,2,2)
m = mkGreenMonster (3,3,3) (4,4,4)

w = [p, m]

main = mapM_ render (filter (isA Player) w)
