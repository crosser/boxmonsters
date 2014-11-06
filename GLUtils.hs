module GLUtils  ( color4d
                , vertex3d
                ) where

import Graphics.Rendering.OpenGL

color4d :: (Double, Double, Double, Double) -> IO ()
color4d (r, g, b, a) =
  color $ Color4 (realToFrac r :: GLfloat)
                 (realToFrac g :: GLfloat)
                 (realToFrac b :: GLfloat)
                 (realToFrac a :: GLfloat)

vertex3d :: (Double, Double, Double) -> IO ()
vertex3d (x, y, z) =
  vertex $ Vertex3 (realToFrac x :: GLfloat)
                   (realToFrac y :: GLfloat)
                   (realToFrac z :: GLfloat)
