{-# LANGUAGE OverloadedStrings #-}

-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://www.wtfpl.net/ for more details.

-- While the previous file, for demonstration purpose, with basically no abstraction,
-- this time we will add several functions to take care of the boilerplate
module Main where

import Data.Foldable
import Data.Function
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.ObjectName
import qualified Data.Vector.Storable as V
import Control.Applicative
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
      S.clear [ColorBuffer, DepthBuffer] *> drawElements Triangles 6 UnsignedInt (intPtrToPtr 3) *>
      glSwapWindow window *>
      gameLoop window

vertices :: V.Vector GLfloat
vertices = V.fromList [0.5, 0.5, 0.0, 0.5, -0.5, 0.0, -0.5, -0.5, 0.0, -0.5, 0.5, 0.0]

indices :: V.Vector GLuint
indices = V.fromList [0,1,3,1,2,3]

loadShaderFromFile shaderType filePath =
  S.createShader shaderType >>= \s ->
  (($=) (shaderSourceBS s) =<< BS.readFile filePath) *>
  compileShader s *>
  pure s

createProgramWith shaders = do
  program <- createProgram
  traverse_ (attachShader program) shaders
  linkProgram program
  pure program

bufferDataWithVector :: (Storable a) => V.Vector a -> BufferTarget -> BufferUsage -> IO ()
bufferDataWithVector v target usage =
  V.unsafeWith v $ \ptr ->
    S.bufferData target $=
    ( fromIntegral $ V.length vertices * sizeOf (undefined `asTypeOf` V.head v)
    , castPtr ptr
    , usage)


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
  bindBuffer ArrayBuffer $= Just vbo
  ebo <- (genObjectName :: IO BufferObject)
  bindBuffer ElementArrayBuffer $= Just ebo
   
  bufferDataWithVector vertices ArrayBuffer StaticDraw
  bufferDataWithVector indices  ElementArrayBuffer StaticDraw
  
  vs <- loadShaderFromFile VertexShader "shader.vert"
  fs <- loadShaderFromFile FragmentShader "shader.frag"
  program <- createProgramWith [vs,fs]  
  deleteObjectName vs
  deleteObjectName fs
  currentProgram $= Just program
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
