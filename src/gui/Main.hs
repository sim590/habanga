
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

import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Data.Text (Text)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Extra ( whileM )
import Control.Concurrent
import Control.Monad.State
import Control.Lens
import qualified GUI.MenuState as M
import qualified GUI.MenuState as M

import System.Environment

import qualified SDL
import SDL.Font (Font)
import qualified SDL.Font as SDLF

newtype KeyboardState = KeyboardState { _pressedKeys :: Set SDL.Keycode
                                      }
makeLenses ''KeyboardState

instance Default KeyboardState where
  def = KeyboardState $ Set.fromList []

data ProgramState = ProgramState { _sdlRenderer   :: SDL.Renderer
                                 , _fontMap       :: Map String Font
                                 , _textureMap    :: Map String SDL.Texture
                                 , _keyboardState :: KeyboardState
                                 , _menuState     :: MenuState
                                 }

makeLenses ''ProgramState

-- Create initial ProgramState with renderer, empty maps, and default settings
initProgramState :: SDL.Renderer -> IO ProgramState
initProgramState renderer = ProgramState renderer mempty mempty def (MenuState 0 True)

-- State transition logic
handleMenuInput :: Event -> ProgramState -> ProgramState
handleMenuInput event (ProgramState renderer _ _ _ (MenuState option open)) = case event of
    KeyPress key -> case key of
        SDLK_SPACE -> closeMenu
        SDLK_w -> changeMenuOption (option - 1) open
        SDLK_s -> changeMenuOption (option + 1) open
        _ -> id
    case event of
        KeyPress key -> case key of
            SDLK_SPACE -> closeMenu
            SDLK_w -> changeMenuOption (option - 1) open
            SDLK_s -> changeMenuOption (option + 1) open
            _ -> id
        KeyDown key -> if open
            then case key of
                Char 'w' -> ProgramState renderer _ _ _ (MenuState (option - 1) True)
                Char 's' -> ProgramState renderer _ _ _ (MenuState (option + 1) True)
                Char ' ' -> ProgramState renderer _ _ _ (MenuState option False)
            else ProgramState renderer _ _ _ (MenuState option True)
        EventMouse MouseButtonLeft Down -> closeMenu
        EventKey (Char '\x1b') Down -> closeMenu
        _ -> ProgramState renderer _ _ _ (MenuState option open)
        EventMouse MouseButtonLeft Down -> closeMenu
        EventKey (Char '\x1b') Down -> closeMenu
        _ -> ProgramState renderer _ _ _ (MenuState option open)


-- TODO: centraliser les variables utiles entre le tui et le gui dans le noyau
_GAME_TITLE_ :: Text
_GAME_TITLE_ = "habanga"

windowConfig :: SDL.WindowConfig
windowConfig = SDL.WindowConfig
  { SDL.windowBorder          = True
  , SDL.windowHighDPI         = False
  , SDL.windowInputGrabbed    = False
  , SDL.windowMode            = SDL.Windowed
  , SDL.windowGraphicsContext = SDL.NoGraphicsContext
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
  homeDir <- liftIO $ getEnv "HOME"
  font    <- SDLF.load (homeDir <> "/.local/share/fonts/mighty-souly-font/MightySouly-lxggD.ttf") 64
  fontMap %= Map.insert "default" font

loadData :: StateT ProgramState IO ()
loadData = loadFonts >> loadTextures

handleEventPayload :: SDL.EventPayload -> StateT ProgramState IO ()
handleEventPayload (SDL.KeyboardEvent ke)
  | SDL.keyboardEventKeyMotion ke == SDL.Pressed  = keyboardState . pressedKeys %= Set.insert (SDL.keysymKeycode (SDL.keyboardEventKeysym ke))
  | SDL.keyboardEventKeyMotion ke == SDL.Released = keyboardState . pressedKeys %= Set.delete (SDL.keysymKeycode (SDL.keyboardEventKeysym ke))
  | otherwise                                     = return ()
handleEventPayload _ = return ()

continueLoop :: StateT ProgramState IO Bool
continueLoop = use (keyboardState . pressedKeys) <&> not . Set.member SDL.KeycodeQ

loop :: StateT ProgramState IO ()
loop = whileM $ do
  renderer <- use sdlRenderer
  SDL.clear renderer

  texture <- use $ textureMap . at "salut" . to fromJust

  events <- SDL.pollEvents
  forM_ events (handleEventPayload . SDL.eventPayload)

  SDL.copy renderer texture Nothing Nothing
  SDL.present renderer
  liftIO $ threadDelay (1000000 `div` 60)

  continueLoop

main :: IO ()
main = do
  SDL.initializeAll
  SDLF.initialize

  window   <- SDL.createWindow _GAME_TITLE_ windowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  execStateT (loadData >> loop) (initProgramState renderer)

  cleanSDL window renderer

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

