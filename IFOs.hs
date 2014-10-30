module IFOs where

type Vector = (Double, Double, Double)

type Position = Vector

type Velocity = Vector

data Player       = Player       Position Velocity
data Projectile   = Projectile   Position Velocity
data GreenMonster = GreenMonster Position Velocity
data RedMonster   = RedMonster   Position Velocity

data IFO          = PlayerFO       Player
                  | ProjectileFO   Projectile
                  | GreenMonsterFO GreenMonster
                  | RedMonsterFO   RedMonster

renderFO :: IFO -> IO ()
renderFO (PlayerFO       p) = render p
renderFO (ProjectileFO   p) = render p
renderFO (GreenMonsterFO p) = render p
renderFO (RedMonsterFO   p) = render p

type World = [IFO]

class Renderable a where
  render :: a -> IO ()

instance Renderable Player where
  render (Player p v)  =
    putStrLn $ "P pos=" ++ (show p) ++ ", vel=" ++ (show v)

instance Renderable Projectile where
  render (Projectile p v)  =
    putStrLn $ "P pos=" ++ (show p) ++ ", vel=" ++ (show v)

instance Renderable RedMonster where
  render (RedMonster p v)  =
    putStrLn $ "P pos=" ++ (show p) ++ ", vel=" ++ (show v)

instance Renderable GreenMonster where
  render (GreenMonster p v) =
    putStrLn $ "G pos=" ++ (show p) ++ ", vel=" ++ (show v)

p = Player (1,1,1) (2,2,2)
m = GreenMonster (3,3,3) (4,4,4)

w = [PlayerFO p, GreenMonsterFO m]

main = mapM_ renderFO w
