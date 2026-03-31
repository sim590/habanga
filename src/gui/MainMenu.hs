
{-|
  Module      : MainMenu
  Description : Menu principal
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  Gestion des événements et rendu du menu principal.
  Ce module suit le même modèle que le TUI : chaque écran expose
  ses propres fonctions @event@ et @render@, appelées par le dispatcher
  dans Main.
-}

module MainMenu
    ( event
    , render
    ) where

import qualified Data.Map as Map

import Foreign.C.Types (CInt)

import Control.Monad.State
import Control.Lens

import ProgramState
import MenuState

import qualified SDL

-- | Gestion des événements du menu principal
event :: SDL.EventPayload -> StateT ProgramState IO ()
event (SDL.KeyboardEvent ke)
  | SDL.keyboardEventKeyMotion ke == SDL.Pressed = do
      isOpen <- use (menuSt . menuOpen)
      let keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
      when isOpen $ case keycode of
        SDL.KeycodeW     -> menuSt . menuOption %= \n -> (n - 1 + menuCount) `mod` menuCount
        SDL.KeycodeUp    -> menuSt . menuOption %= \n -> (n - 1 + menuCount) `mod` menuCount
        SDL.KeycodeS     -> menuSt . menuOption %= \n -> (n + 1) `mod` menuCount
        SDL.KeycodeDown  -> menuSt . menuOption %= \n -> (n + 1) `mod` menuCount
        SDL.KeycodeSpace -> do
          idx <- use (menuSt . menuOption)
          handleAction (menuActions idx)
        SDL.KeycodeReturn -> do
          idx <- use (menuSt . menuOption)
          handleAction (menuActions idx)
        _                 -> return ()
  | otherwise = return ()
event (SDL.MouseButtonEvent mbe)
  | SDL.mouseButtonEventMotion mbe == SDL.Pressed
  , SDL.mouseButtonEventButton mbe == SDL.ButtonLeft = do
      isOpen <- use (menuSt . menuOpen)
      -- TODO: implémenter la détection du clic sur un item du menu
      --       (nécessite de connaître les coordonnées de chaque item)
      when isOpen $ return ()
event _ = return ()

-- | Exécuter l'action correspondant à l'option du menu sélectionnée
handleAction :: MenuAction -> StateT ProgramState IO ()
handleAction NewGame  = menuSt . menuOpen .= False -- TODO: démarrer une nouvelle partie
handleAction Continue = menuSt . menuOpen .= False -- TODO: continuer la partie en cours
handleAction Options  = return ()                  -- TODO: ouvrir le sous-menu d'options
handleAction QuitGame = menuSt . menuOpen .= False -- Sera capté par continueLoop

-- | Rendu du menu principal
render :: StateT ProgramState IO ()
render = do
  renderer    <- use sdlRenderer
  selectedIdx <- use (menuSt . menuOption)
  textures    <- use textureMap

  -- Fond d'écran
  case Map.lookup "menu-bg" textures of
    Just bgTex -> SDL.copy renderer bgTex Nothing Nothing
    Nothing    -> do
      SDL.rendererDrawColor renderer SDL.$= SDL.V4 35 35 50 255
      SDL.clear renderer

  -- Titre centré en haut
  case Map.lookup "menu-title" textures of
    Just titleTex -> drawCentered renderer titleTex 1280 120
    Nothing       -> return ()

  -- Options du menu
  forM_ (zip [0..] menuTexts) $ \ (i, _) -> do
    let key = if i == selectedIdx
              then "menu-sel-" <> show i
              else "menu-"     <> show i
        y   = 300 + fromIntegral i * 80
    case Map.lookup key textures of
      Just tex -> drawCentered renderer tex 1280 y
      Nothing  -> return ()

-- | Dessiner une texture centrée horizontalement à une position Y donnée
drawCentered :: SDL.Renderer -> SDL.Texture -> CInt -> CInt -> StateT ProgramState IO ()
drawCentered renderer tex screenW y = do
  info <- SDL.queryTexture tex
  let w = SDL.textureWidth info
      h = SDL.textureHeight info
      x = (screenW - w) `div` 2
  SDL.copy renderer tex Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h))

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

