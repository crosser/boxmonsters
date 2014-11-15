{-# LANGUAGE Arrows #-}

module Monster (Monster(..), monstersWire, renderMonster) where

import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import GLUtils
import IFOs

data MonsterLevel = GreenMonster | RedMonster
data Monster = Monster LocVel MonsterLevel

hv = 0.05

-- Functional part.

monsterWire :: Wire s () IO a Monster
monsterWire = pure $ Monster ((0,0,1),(0,0,1)) GreenMonster
monstersWire = (:[]) <$> monsterWire

-- Rendering Monster.

renderFace :: Vec3 -> IO ()
renderFace (x, y, z) =
  renderPrimitive Quads $ mapM_ vertex3d
    [ (x-hv, y-hv, z)
    , (x+hv, y-hv, z)
    , (x+hv, y+hv, z)
    , (x-hv, y+hv, z)
    ]

renderMonster :: (Double, Double) -> Monster -> IO ()
renderMonster _ (Monster (loc, _) level) = do
  -- matrixMode $= Modelview 0
  -- loadIdentity
  case level of
    GreenMonster -> color4d (0, 1, 0, 1)
    RedMonster -> color4d (1, 0, 0, 1)
  renderFace loc
