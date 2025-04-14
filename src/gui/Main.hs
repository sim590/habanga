
{-|
  Module      : Main
  Description : Point d'entrée du jeu
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  Point d'entrée de l'interface graphique SDL du jeu Habanga.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Text (Text)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Extra ( whileM )
import Control.Concurrent
import Control.Monad.State
import Control.Lens

import qualified SDL
import SDL.Font (Font)
import qualified SDL.Font as SDLF

data ProgramState = ProgramState { _sdlRenderer :: SDL.Renderer
                                 , _fontMap     :: Map String Font
                                 , _textureMap    :: Map String SDL.Texture
                                 }
makeLenses ''ProgramState

-- TODO: centraliser les variables utiles entre le tui et le gui dans le noyau
_GAME_TITLE_ :: Text
_GAME_TITLE_ = "habanga"

windowConfig :: SDL.WindowConfig
windowConfig = SDL.WindowConfig
    { SDL.windowBorder          = True
    , SDL.windowHighDPI         = False
    , SDL.windowInputGrabbed    = False
    , SDL.windowMode            = SDL.Windowed
    , SDL.windowGraphicsContext = SDL.VulkanContext
    , SDL.windowPosition        = SDL.Wherever
    , SDL.windowResizable       = False
    , SDL.windowInitialSize     = SDL.V2 1280 720
    , SDL.windowVisible         = True
    }

cleanSDL :: SDL.Window -> SDL.Renderer -> IO ()
cleanSDL window renderer = do
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDLF.quit

-- TODO: les textures devraient sûrement être placées dans un module à part.
-- Toutes ces textures seront ensuite chargées dans l'état du programme.
loadTextures :: StateT ProgramState IO ()
loadTextures = do
  let white = SDL.V4 255 255 255 255
  renderer <- use sdlRenderer
  fonts    <- use fontMap
  surf     <- SDLF.solid (fonts Map.! "default") white "salut SDL!"
  texture  <- SDL.createTextureFromSurface renderer surf
  textureMap %= Map.insert "salut" texture

loadFonts :: StateT ProgramState IO ()
loadFonts = do
  font <- SDLF.load "/usr/share/fonts/Adwaita/AdwaitaSans-Regular.ttf" 12
  fontMap %= Map.insert "default" font

loadData :: StateT ProgramState IO ()
loadData = loadFonts >> loadTextures

loop :: StateT ProgramState IO ()
loop = whileM $ do
  renderer <- use sdlRenderer
  texture <- use $ textureMap . at "salut" . to fromJust

  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer
  liftIO $ threadDelay (1000000 `div` 60)
  return True

main :: IO ()
main = do
  SDL.initializeAll
  SDLF.initialize

  window   <- SDL.createWindow _GAME_TITLE_ windowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  finalState <- execStateT (loadData >> loop) (ProgramState renderer mempty mempty)

  cleanSDL window renderer

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

