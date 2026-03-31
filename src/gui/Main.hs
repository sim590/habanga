
{-|
  Module      : Main
  Description : Point d'entrée du jeu
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  Point d'entrée de l'interface graphique SDL du jeu Habanga.
  Ce module est un pur dispatcher : il route les événements et le rendu
  vers les sous-modules appropriés (MainMenu, futur GameView, etc.)
  selon l'état courant du programme.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}

module Main where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Map as Map

import Control.Monad.Extra ( whileM )
import Control.Concurrent
import Control.Monad.State
import Control.Lens

import ProgramState
import MenuState
import qualified MainMenu
import Textures.Menu (loadMenuTextures)

import System.Environment

import qualified SDL
import qualified SDL.Font as SDLF

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

loadFonts :: StateT ProgramState IO ()
loadFonts = do
  homeDir <- liftIO $ getEnv "HOME"
  font    <- SDLF.load (homeDir <> "/.local/share/fonts/mighty-souly-font/MightySouly-lxggD.ttf") 64
  fontMap %= Map.insert "default" font

loadData :: StateT ProgramState IO ()
loadData = loadFonts >> loadMenuTextures

-- | Dispatcher d'événements : route vers le module approprié selon l'état
handleEventPayload :: SDL.EventPayload -> StateT ProgramState IO ()
handleEventPayload payload = do
  isMenuOpen <- use (menuSt . menuOpen)
  if | isMenuOpen -> MainMenu.event payload
     -- TODO: ajouter ici les futures branches (ex: GameView.event)
     | otherwise  -> do
         -- Quand le menu est fermé, gérer les inputs de gameplay
         -- TODO: déléguer vers un futur GameView.event
         case payload of
           SDL.KeyboardEvent ke
             | SDL.keyboardEventKeyMotion ke == SDL.Pressed ->
                 let keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
                 in case keycode of
                   SDL.KeycodeEscape -> menuSt . menuOpen .= True
                   _                 -> keyboardState . pressedKeys %= Set.insert keycode
             | SDL.keyboardEventKeyMotion ke == SDL.Released ->
                 keyboardState . pressedKeys %= Set.delete (SDL.keysymKeycode (SDL.keyboardEventKeysym ke))
             | otherwise -> return ()
           _ -> return ()

continueLoop :: StateT ProgramState IO Bool
continueLoop = do
  isMenuOpen <- use (menuSt . menuOpen)
  if | isMenuOpen -> return True  -- Le menu est ouvert, on continue la boucle
     -- TODO: ajouter ici les futures branches
     | otherwise  -> do
         keys <- use (keyboardState . pressedKeys)
         idx  <- use (menuSt . menuOption)
         -- Quitter si Q est pressé ou si QuitGame a été sélectionné
         return $ not (Set.member SDL.KeycodeQ keys) && menuActions idx /= QuitGame

loop :: StateT ProgramState IO ()
loop = whileM $ do
  renderer <- use sdlRenderer

  events <- SDL.pollEvents
  forM_ events (handleEventPayload . SDL.eventPayload)

  isMenuOpen <- use (menuSt . menuOpen)

  -- Dispatcher de rendu : route vers le module approprié
  if | isMenuOpen -> MainMenu.render
     -- TODO: ajouter ici les futures branches (ex: GameView.render)
     | otherwise  -> do
         -- Rendu gameplay normal
         -- TODO: déléguer vers un futur GameView.render
         SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
         SDL.clear renderer

  SDL.present renderer
  liftIO $ threadDelay (1000000 `div` 60)

  continueLoop

main :: IO ()
main = do
  SDL.initializeAll
  SDLF.initialize

  window   <- SDL.createWindow _GAME_TITLE_ windowConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  _ <- execStateT (loadData >> loop) (initProgramState renderer)

  cleanSDL window renderer

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

