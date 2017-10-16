{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
-- This program is free software. It comes without any warranty, to
-- the extent permitted by applicable law. You can redistribute it
-- and/or modify it under the terms of the Do What The Fuck You Want
-- To Public License, Version 2, as published by Sam Hocevar. See
-- http://www.wtfpl.net/ for more details.



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
import Graphics.Rendering.OpenGL hiding (perspective,lookAt,Vector3,Front,Back,Left,Right,normalize)
import SDL hiding(perspective,clear,normalize)
import SDL.Input.Mouse
import System.IO
import Codec.Picture.Extra
import Linear.Quaternion
import Linear.Matrix
import Linear.Projection
import FRP.Yampa.Vector3

profile :: Profile
profile = Core Normal 3 3

data InputEvent
  = QuitP
  | FrontD
  | BackD
  | LeftD
  | RightD
  deriving (Show, Eq)


crossV :: RealFloat a => Vector3 a -> Vector3 a -> Vector3 a
crossV v3 v3' = case (vector3XYZ v3,vector3XYZ v3') of
                  ((a,b,c),(d,e,f)) -> vector3 (b*f-c*e) (c*d-a*f) (a*e-b*d)

data OutputEvent
  = QuitG
  deriving (Show, Eq)

data ObjectState = ObjectState {pos::Vector3 Float, direct :: Vector3 Float}
newtype CursorOffset = CursorOffset (Float,Float)

toFPSVec v3 = case vector3XYZ v3 of
                (x,y,z) -> vector3 x 0 z

e2v :: Vector3 Float -> Vector3 Float -> InputEvent -> Vector3 Float
e2v front right gi =
  case gi of
    BackD -> Y.negateVector front
    FrontD -> front
    LeftD -> Y.negateVector right
    RightD -> right    
    _ -> Y.zeroVector

sig :: Y.SF (CursorOffset,[InputEvent]) (Float, Maybe OutputEvent, ObjectState)
sig = proc ((CursorOffset (xOff,yOff)),gis) -> do
    maybeQuit <- quitArr -< gis
    yaw <- mouseArr -< xOff
    pitch <- mouseArr -< yOff
    let cameraFront = Y.normalize $ vector3 ((cos pitch) * (cos yaw)) (sin pitch) ((cos pitch)* (sin yaw))
        cameraRight = Y.normalize $ crossV cameraFront cameraUp
        pe2v = e2v cameraFront cameraRight
        f :: InputEvent -> Vector3 Float -> Vector3 Float
        f e acc = acc Y.^+^ pe2v e
        moveDirection = let v = foldr f (vector3 0 0 0) gis
                        in if v == Y.zeroVector
                              then v
                              else Y.normalize v
    position <- Y.integral -< cameraSpeed Y.*^ moveDirection
    angle <- Y.localTime -< ()
    Y.returnA -< (double2Float angle, maybeQuit, ObjectState {pos = position, direct = cameraFront})
  where quitArr = Y.arr $ \events -> if any (==QuitP) events then Just QuitG else Nothing
        mouseArr = Y.arr (*mouseSensitivity) Y.>>> Y.integral
        boundPitch pitch
         | pitch > pi/2-0.1 = pi/2-0.1
         | pitch < -pi/2+0.1 = -pi/2 + 0.1
         | otherwise = pitch
        cameraUp = vector3 0 1 0
        

mouseSensitivity :: Float          
mouseSensitivity = 0.5


cubePositions :: [V3 GLfloat]
cubePositions = fmap (\(x,y,z) -> V3 x y z) [
                      ( 0.0,  0.0,  0.0), 
                      ( 2.0,  5.0, -15.0), 
                      (-1.5, -2.2, -2.5),  
                      (-3.8, -2.0, -12.3),  
                      ( 2.4, -0.4, -3.5),  
                      (-1.7,  3.0, -7.5),  
                      ( 1.3, -2.0, -2.5),  
                      ( 1.5,  2.0, -2.5), 
                      ( 1.5,  0.2, -1.5), 
                      (-1.3,  1.0, -1.5)  
                ]

angleOffsets = scanl (+) 0 [pi/9 ..]

drawCubes :: M44 GLfloat -> GLfloat -> UniformLocation -> IO ()
drawCubes view angle loc = traverse_ draw (zip cubePositions angleOffsets)
  where draw (t,aOffset) = do
          (toMatrix (proj!*!view !*!(model (angle+aOffset) t)) :: IO (GLmatrix GLfloat))>>= (uniform loc $=)
          drawArrays Triangles 0 36

scancodeToInputEvent ScancodeW = FrontD
scancodeToInputEvent ScancodeS = BackD
scancodeToInputEvent ScancodeA = LeftD
scancodeToInputEvent ScancodeD = RightD

sense mouseRef timeRef _ = do
  P (V2 x y) <- getRelativeMouseLocation
  -- P (V2 xOld yOld) <- readIORef mouseRef
  -- let xOff = fromIntegral x - fromIntegral xOld
  --     yOff = fromIntegral yOld - fromIntegral y
  -- putStrLn . show $ (xOff,yOff)
  writeIORef mouseRef (P (V2 x y))
  -- warpMouse WarpCurrentFocus (P (V2 (screenWidth/2) (screenHeight/2) ))
  now <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now
  quit <- pure . any ((==) <$> eventPayload <*> pure QuitEvent) =<< pollEvents
  let dt = realToFrac $ now `diffUTCTime` lastTime
  keyF <-getKeyboardState
  let events = (if quit then [QuitP] else []) ++ (fmap scancodeToInputEvent . filter keyF $ [ScancodeW, ScancodeS, ScancodeA, ScancodeD])
  pure (dt,Just (CursorOffset (fromIntegral x ,fromIntegral (-y)),events))



initialize :: IO (CursorOffset,[InputEvent])
initialize = pure (curry CursorOffset 0 0,[])

newtype CameraState = CameraState (V3 GLfloat)

toV3 :: (RealFloat a) => Vector3 a -> V3 a
toV3 v = let (x,y,z) = vector3XYZ v
         in V3 x y z

actuate :: UniformLocation -> Window -> Bool -> (Float, Maybe OutputEvent, ObjectState) -> IO Bool
actuate loc window _ (t, oe, ObjectState {pos = position, direct = direction}) =
  if oe == Just QuitG
    then pure True
    else clear [ColorBuffer, DepthBuffer] *>
         drawCubes (lookAt (toV3 position) (toV3 position + toV3 direction) (V3 0 1 0) ) t loc *>
         glSwapWindow window *>
         (traverse_ (putStrLn . show) <$> (get errors)) *>
         pure False

vertices :: V.Vector GLfloat
vertices = V.fromList [
                        -0.5, -0.5, -0.5,  0.0, 0.0,
                         0.5, -0.5, -0.5,  1.0, 0.0,
                         0.5,  0.5, -0.5,  1.0, 1.0,
                         0.5,  0.5, -0.5,  1.0, 1.0,
                        -0.5,  0.5, -0.5,  0.0, 1.0,
                        -0.5, -0.5, -0.5,  0.0, 0.0,

                        -0.5, -0.5,  0.5,  0.0, 0.0,
                         0.5, -0.5,  0.5,  1.0, 0.0,
                         0.5,  0.5,  0.5,  1.0, 1.0,
                         0.5,  0.5,  0.5,  1.0, 1.0,
                        -0.5,  0.5,  0.5,  0.0, 1.0,
                        -0.5, -0.5,  0.5,  0.0, 0.0,

                        -0.5,  0.5,  0.5,  1.0, 0.0,
                        -0.5,  0.5, -0.5,  1.0, 1.0,
                        -0.5, -0.5, -0.5,  0.0, 1.0,
                        -0.5, -0.5, -0.5,  0.0, 1.0,
                        -0.5, -0.5,  0.5,  0.0, 0.0,
                        -0.5,  0.5,  0.5,  1.0, 0.0,

                         0.5,  0.5,  0.5,  1.0, 0.0,
                         0.5,  0.5, -0.5,  1.0, 1.0,
                         0.5, -0.5, -0.5,  0.0, 1.0,
                         0.5, -0.5, -0.5,  0.0, 1.0,
                         0.5, -0.5,  0.5,  0.0, 0.0,
                         0.5,  0.5,  0.5,  1.0, 0.0,

                        -0.5, -0.5, -0.5,  0.0, 1.0,
                         0.5, -0.5, -0.5,  1.0, 1.0,
                         0.5, -0.5,  0.5,  1.0, 0.0,
                         0.5, -0.5,  0.5,  1.0, 0.0,
                        -0.5, -0.5,  0.5,  0.0, 0.0,
                        -0.5, -0.5, -0.5,  0.0, 1.0,

                        -0.5,  0.5, -0.5,  0.0, 1.0,
                         0.5,  0.5, -0.5,  1.0, 1.0,
                         0.5,  0.5,  0.5,  1.0, 0.0,
                         0.5,  0.5,  0.5,  1.0, 0.0,
                        -0.5,  0.5,  0.5,  0.0, 0.0,
                        -0.5,  0.5, -0.5,  0.0, 1.0
           ]


scale :: (Num a) => a -> a-> a -> M44 a
scale sx sy sz =
  V4 (V4 sx 0 0 0) (V4 0 sy 0 0) (V4 0 0 sz 0) (V4 0 0 0 1)

translationM :: (Num a) => a -> a -> a -> M44 a
translationM x y z =
  V4 (V4 1 0 0 x) (V4 0 1 0 y) (V4 0 0 1 z) (V4 0 0 0 1)


cameraSpeed :: GLfloat
cameraSpeed = 10


cameraUp :: V3 GLfloat
cameraUp = V3 0 1 0

view :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> M44 GLfloat
view pos front up = lookAt pos (pos + front) up


model :: GLfloat -> V3 GLfloat -> M44 GLfloat
model angle t = let r = axisAngle (V3 0.5 1 0) angle
                   in mkTransformation r t


indices :: V.Vector GLuint
indices = V.fromList [0, 1, 3, 1, 2, 3]

loadShaderFromFile shaderType filePath =
  createShader shaderType >>= \s ->
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
    bufferData target $=
    ( fromIntegral $ V.length vertices * sizeOf (undefined `asTypeOf` V.head v)
    , castPtr ptr
    , usage)

proj :: M44 GLfloat
proj = perspective (pi/4) (4/3) 0.1 100


-- This function might look cryptic if you've never dealt with FFI before.
-- Check Foreign.Storable to get more information on how this works
toMatrix :: (Matrix m, MatrixComponent c) => M44 c -> IO (m c)
toMatrix mr =
  let mc = transpose mr
  in withNewMatrix ColumnMajor (flip poke mc . castPtr)

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

glConfig = defaultOpenGL {glProfile = profile, glMultisampleSamples = 8}

screenWidth = 1024
screenHeight = 768

windowConfig = defaultWindow
  {windowOpenGL = Just glConfig,
   windowInputGrabbed = True,
   windowInitialSize = V2 screenWidth screenHeight
  }

initializeSDL :: IO Window
initializeSDL = do
  initializeAll
  createWindow "cameraKeyboardDt" windowConfig

main :: IO ()
main
  -- Initialize SDL
 = do
  window<-initializeSDL
  -- Create OpenGL Context
  glCreateContext window

  multisample $= Enabled

  -- Print max vertex attributes available
  hPutStrLn stderr =<< show <$> get maxVertexAttribs
  hPutStrLn stderr =<< show <$> get maxTextureUnit
  -- vao,vbo
  vao <- (genObjectName :: IO VertexArrayObject)
  bindVertexArrayObject $= Just vao
  vbo <- (genObjectName :: IO BufferObject)
  bindBuffer ArrayBuffer $= Just vbo 
  bufferDataWithVector vertices ArrayBuffer StaticDraw
  bufferDataWithVector indices ElementArrayBuffer StaticDraw
  -- Enable depth buffer
  depthFunc $= Just Less
  -- Load shaders
  vs <- loadShaderFromFile VertexShader "cameraKeyboardDt.vert"
  fs <- loadShaderFromFile FragmentShader "cameraKeyboardDt.frag"
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
        (fromIntegral $ 5 * sizeOf (undefined :: GLfloat))
        (intPtrToPtr 0))

  vertexAttribPointer (AttribLocation 1) $=
    ( ToFloat
    , VertexArrayDescriptor
        2
        Float
        (fromIntegral $ 5 * sizeOf (undefined :: GLfloat))
        (intPtrToPtr (fromIntegral $ 3 * sizeOf (undefined :: GLfloat))))
  vertexAttribArray (AttribLocation 0) $= Enabled
  vertexAttribArray (AttribLocation 1) $= Enabled
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
  loc2 <- get . uniformLocation program $ "transform"


  uniform loc0 $= TextureUnit 0
  uniform loc1 $= TextureUnit 1


  -- Set clearColor to #66ccff
  clearColor $= Color4 0.4 0.8 1.0 1.0
  timeRef <- newIORef =<< getCurrentTime
  mouseRef <- newIORef =<< getRelativeMouseLocation
  Y.reactimate Main.initialize (sense mouseRef timeRef) (actuate loc2 window) sig
