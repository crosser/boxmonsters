{-# LANGUAGE Arrows #-}

module Monster (Monster(..), monsterWire, renderMonster) where

import Prelude hiding ((.), id)
import Control.Monad.Fix
import Control.Wire
import FRP.Netwire

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import GLUtils
import IFOs

data MonsterLevel = GreenMonster | RedMonster
data Monster = Monster LocVel MonsterLevel

hvsize = 0.05

-- Functional part.

monsterWire :: (HasTime t s, Monoid e, MonadFix m)
             => LocVel
             -> Wire s e m Inputs Monster
monsterWire ilv = Monster <$> bouncylv hvsize ilv <*> pure GreenMonster

-- Rendering Monster.

renderFace :: Loc -> IO ()
renderFace (V3 x y z) =
  renderPrimitive Quads $ mapM_ vertex3d
    [ (x-hvsize, y-hvsize, z)
    , (x+hvsize, y-hvsize, z)
    , (x+hvsize, y+hvsize, z)
    , (x-hvsize, y+hvsize, z)
    ]

renderMonster :: V3 Double -> Monster -> IO ()
renderMonster _ (Monster (loc, _) level) = do
  -- matrixMode $= Modelview 0
  -- loadIdentity
  case level of
    GreenMonster -> color4d (0, 1, 0, 1)
    RedMonster -> color4d (1, 0, 0, 1)
  renderFace loc
