{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Applicative
import SDL
import Graphics.Rendering.OpenGL as S

profile :: Profile
profile = Core Normal 3 3


gameLoop window  = do
    quit <- pure . any ((==) <$> eventPayload <*> pure QuitEvent)  =<< pollEvents
    case quit of
      True -> pure ()
      False -> S.clear [ColorBuffer, DepthBuffer] >> glSwapWindow window >> gameLoop window 
  


main :: IO ()
main = do
  initializeAll
  window <- createWindow "Goodbye, world" defaultWindow{windowOpenGL=Just defaultOpenGL {glProfile = profile}}
  glCreateContext window
  clearColor $= S.Color4 0.4 0.8 1.0 1.0
  gameLoop window
  
