{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.ObjectName
import qualified Data.Vector.Storable as V
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL as S
import SDL

profile :: Profile
profile = Core Normal 3 3

gameLoop window = do
  quit <- pure . any ((==) <$> eventPayload <*> pure QuitEvent) =<< pollEvents
  case quit of
    True -> pure ()
    False ->
      S.clear [ColorBuffer, DepthBuffer] >> drawArrays Triangles 0 3 >> glSwapWindow window >>
      gameLoop window

vertices :: V.Vector GLfloat
vertices = V.fromList [-0.5, -0.5, 0.0, 0.5, -0.5, 0.0, 0.0, 0.5, 0.0]

main :: IO ()
main = do
  initializeAll
  window <-
    createWindow
      "Goodbye, world"
      defaultWindow {windowOpenGL = Just defaultOpenGL {glProfile = profile}}
  glCreateContext window
  vao <- (genObjectName :: IO VertexArrayObject)

  bindVertexArrayObject $= Just vao

  vbo <- (genObjectName :: IO BufferObject)
  S.bindBuffer S.ArrayBuffer $= Just vbo
  V.unsafeWith vertices $ \ptr ->
    S.bufferData S.ArrayBuffer $=
    ( fromIntegral $ V.length vertices * sizeOf (undefined :: GLfloat)
    , castPtr ptr
    , S.StaticDraw)
  vs <- S.createShader VertexShader
  vsSource <- BS.readFile "shader.vert"
  shaderSourceBS vs $= vsSource
  compileShader vs

  putStrLn =<< shaderInfoLog vs

  fs <- S.createShader FragmentShader
  ($=) (shaderSourceBS fs) =<< BS.readFile "shader.frag"
  compileShader fs

  putStrLn =<< shaderInfoLog fs

  program <- createProgram
  attachShader program vs
  attachShader program fs
  linkProgram program
  currentProgram $= Just program
  deleteObjectName vs
  deleteObjectName fs
  vertexAttribPointer (AttribLocation 0) $=
    ( ToFloat
    , VertexArrayDescriptor
        3
        Float
        (fromIntegral $ 3 * sizeOf (undefined :: GLfloat))
        (intPtrToPtr 0))
  vertexAttribArray (AttribLocation 0) $= Enabled
  clearColor $= S.Color4 0.4 0.8 1.0 1.0
  gameLoop window
