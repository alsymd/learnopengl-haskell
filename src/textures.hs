{-# LANGUAGE OverloadedStrings #-}

-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://www.wtfpl.net/ for more details.
-- While the previous file, for demonstration purpose, with basically no abstraction,
-- this time we will add several functions to take care of the boilerplate
module Main where

import Codec.Picture
import Codec.Picture.Types
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Function
import Data.IORef
import Data.ObjectName
import Data.Time.Clock
import qualified Data.Vector.Storable as V
import qualified FRP.Yampa as Y
import Foreign.Ptr
import Foreign.Storable
import GHC.Float
import Graphics.Rendering.OpenGL as S
import SDL
import System.IO
import Codec.Picture.Extra

profile :: Profile
profile = Core Normal 3 3

data InputEvent
  = Quit
  | Cont
  deriving (Show, Eq)

sig :: Y.SF InputEvent (Float, InputEvent)
sig =
  (Y.localTime Y.>>> Y.arr double2Float Y.>>> Y.arr ((+ 0.5) . (/ 2) . sin)) Y.&&&
  Y.identity

sense timeRef _ = do
  now <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  quit <- pure . any ((==) <$> eventPayload <*> pure QuitEvent) =<< pollEvents
  let dt = now `diffUTCTime` lastTime
  pure (,) <*> pure (realToFrac dt) <*>
    pure
      (if quit
         then Just Quit
         else Just Cont)

initialize :: IO InputEvent
initialize = pure Cont

actuate :: Window -> Bool -> (Float, InputEvent) -> IO Bool
actuate window _ (t, s) =
  if s == Quit
    then pure True
    else S.clear [ColorBuffer, DepthBuffer] *>
         drawElements Triangles 6 UnsignedInt (intPtrToPtr 3) *>
         glSwapWindow window *>
         pure False

vertices :: V.Vector GLfloat
vertices =
  V.fromList
    [ 0.5
    , 0.5
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , 1.0
    , 1.0
    , 0.5
    , -0.5
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 1.0
    , 0.0
    , -0.5
    , -0.5
    , 0.0
    , 0.0
    , 0.0
    , 1.0
    , 0.0
    , 0.0
    , -0.5
    , 0.5
    , 0.0
    , 1.0
    , 1.0
    , 0.0
    , 0.0
    , 1.0
    ]

indices :: V.Vector GLuint
indices = V.fromList [0, 1, 3, 1, 2, 3]

loadShaderFromFile shaderType filePath =
  S.createShader shaderType >>= \s ->
    (($=) (shaderSourceBS s) =<< BS.readFile filePath) *> compileShader s *>
    pure s

createProgramWith shaders = do
  program <- createProgram
  traverse_ (attachShader program) shaders
  linkProgram program
  pure program

bufferDataWithVector ::
     (Storable a) => V.Vector a -> BufferTarget -> BufferUsage -> IO ()
bufferDataWithVector v target usage =
  V.unsafeWith v $ \ptr ->
    S.bufferData target $=
    ( fromIntegral $ V.length vertices * sizeOf (undefined `asTypeOf` V.head v)
    , castPtr ptr
    , usage)

createTextureFromFile :: FilePath -> IO (Either String TextureObject)
createTextureFromFile filePath = do
  ei <- readImage filePath
  case ei of
    Left str -> pure . Left $ str
    Right dynamicImage -> do
      case dynamicImage of
        ImageYCbCr8 jimg -> do
          let img = flipVertically . convertImage  $ jimg :: Image PixelRGB8
          tex <- (genObjectName :: IO TextureObject)
          textureBinding Texture2D $= Just tex
          V.unsafeWith (imageData img) $
            texImage2D
              Texture2D
              NoProxy
              0
              RGB8
              (TextureSize2D
                 (fromIntegral . imageWidth $ img)
                 (fromIntegral . Codec.Picture.imageHeight $ img))
              0 .
            PixelData RGB UnsignedByte . castPtr
          generateMipmap' Texture2D
          textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
          textureBinding Texture2D $= Nothing
          pure $ Right tex
        ImageRGBA8 jimg -> do
          let img = flipVertically jimg
          tex <- (genObjectName :: IO TextureObject)
          textureBinding Texture2D $= Just tex
          V.unsafeWith (imageData img) $
            texImage2D
              Texture2D
              NoProxy
              0
              RGBA8
              (TextureSize2D
                 (fromIntegral . imageWidth $ img)
                 (fromIntegral . Codec.Picture.imageHeight $ img))
              0 .
            PixelData RGBA UnsignedByte . castPtr
          generateMipmap' Texture2D
          textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
          textureBinding Texture2D $= Nothing
          pure $ Right tex
        otherwise -> pure . Left $ "Unrecognized format"

main :: IO ()
main
  -- Initialize SDL
 = do
  initializeAll
  window <-
    createWindow
      "Color"
      defaultWindow {windowOpenGL = Just defaultOpenGL {glProfile = profile}}
  -- Create OpenGL Context
  glCreateContext window
  -- Print max vertex attributes available
  hPutStrLn stderr =<< show <$> get maxVertexAttribs
  hPutStrLn stderr =<< show <$> get maxTextureUnit
  -- vao,vbo,ebo
  vao <- (genObjectName :: IO VertexArrayObject)
  bindVertexArrayObject $= Just vao
  vbo <- (genObjectName :: IO BufferObject)
  bindBuffer ArrayBuffer $= Just vbo
  ebo <- (genObjectName :: IO BufferObject)
  bindBuffer ElementArrayBuffer $= Just ebo
  bufferDataWithVector vertices ArrayBuffer StaticDraw
  bufferDataWithVector indices ElementArrayBuffer StaticDraw
  -- Load shaders
  vs <- loadShaderFromFile VertexShader "textures.vert"
  fs <- loadShaderFromFile FragmentShader "textures.frag"
  program <- createProgramWith [vs, fs]
  deleteObjectName vs
  deleteObjectName fs
  currentProgram $= Just program
  -- Specify vertex attributes
  vertexAttribPointer (AttribLocation 0) $=
    ( ToFloat
    , VertexArrayDescriptor
        3
        Float
        (fromIntegral $ 8 * sizeOf (undefined :: GLfloat))
        (intPtrToPtr 0))
  vertexAttribPointer (AttribLocation 1) $=
    ( ToFloat
    , VertexArrayDescriptor
        3
        Float
        (fromIntegral $ 8 * sizeOf (undefined :: GLfloat))
        (intPtrToPtr (fromIntegral $ 3 * sizeOf (undefined :: GLfloat))))
  vertexAttribPointer (AttribLocation 2) $=
    ( ToFloat
    , VertexArrayDescriptor
        2
        Float
        (fromIntegral $ 8 * sizeOf (undefined :: GLfloat))
        (intPtrToPtr (fromIntegral $ 6 * sizeOf (undefined :: GLfloat))))
  vertexAttribArray (AttribLocation 0) $= Enabled
  vertexAttribArray (AttribLocation 1) $= Enabled
  vertexAttribArray (AttribLocation 2) $= Enabled
  -- Load texture
  Right texture0 <- createTextureFromFile "container.jpg"
  Right texture1 <- createTextureFromFile "awesomeface.png"
  -- Print out error message
  traverse_ (putStrLn . show) <$> (get errors)
  -- Bind texture
  textureBinding Texture2D $= Just texture0
  activeTexture $= TextureUnit 1
  textureBinding Texture2D $= Just texture1

  -- Set texture uniform
  loc0 <- get . uniformLocation program $ "texture1"
  loc1 <- get . uniformLocation program $ "texture2"

  uniform loc0 $= TextureUnit 0
  uniform loc1 $= TextureUnit 1


  -- Set clearColor to #66ccff
  clearColor $= S.Color4 0.4 0.8 1.0 1.0
  timeRef <- newIORef =<< getCurrentTime
  Y.reactimate Main.initialize (sense timeRef) (actuate window) sig
