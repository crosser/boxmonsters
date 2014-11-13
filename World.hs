module World where

import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import IFOs
import Player

-- to me moved to their respective modules
data MonsterClass = GreenMonster | RedMonster
data Monster = Monster PosVel MonsterClass
monsterWire :: Wire s () IO a Monster
monsterWire = pure $ Monster ((0,0,0),(0,0,0)) GreenMonster
data Projectile = Projectile PosVel
projectileWire :: Wire s () IO a Projectile
projectileWire = pure $ Projectile ((0,0,0),(0,0,0))
-- end of to me moved to their respective modules

data World = World Player [Monster] [Projectile]

worldWire :: (HasTime t s) => Wire s () IO a World
-- for now, make it of just a single Player element
worldWire = World <$> playerWire
                  <*> ((:[]) <$> monsterWire)
                  <*> ((:[]) <$> projectileWire)

renderWorld :: Size -> World -> IO ()
renderWorld size (World player monsters projectiles) = do
  clear [ColorBuffer, DepthBuffer]
  renderPlayer player
  -- mapM_ renderMonster monsters
  -- mapM_ renderProjectile projectiles
  renderHUD player
  swapBuffers
