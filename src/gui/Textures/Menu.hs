
{-|
  Module      : Textures.Menu
  Description : Textures du menu principal
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  Chargement des textures utilisées par le menu principal (fond d'écran,
  titre, options normales et sélectionnées).
-}

{-# LANGUAGE TemplateHaskell #-}

module Textures.Menu
    ( loadMenuTextures
    ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile, makeRelativeToProject)

import Control.Monad.State
import Control.Lens

import qualified SDL
import qualified SDL.Font as SDLF

import ProgramState
import MenuState
import Textures (loadEmbeddedPNG)

-- | Image de fond du menu, imbriquée dans le binaire à la compilation
bgImageData :: BS.ByteString
bgImageData = $(makeRelativeToProject "resources/habanga-gui/fond-ecran.png" >>= embedFile)

-- | Charger les textures du menu principal
loadMenuTextures :: StateT ProgramState IO ()
loadMenuTextures = do
  let white  = SDL.V4 255 255 255 255
      yellow = SDL.V4 255 220 0   255
  renderer <- use sdlRenderer
  fonts    <- use fontMap
  let font = fonts Map.! "default"
  -- Fond d'écran du menu (imbriqué dans le binaire)
  bgTex <- liftIO $ loadEmbeddedPNG renderer bgImageData
  textureMap %= Map.insert "menu-bg" bgTex
  -- Titre
  titleSurf <- SDLF.solid font (SDL.V4 255 180 0 255) (T.pack "HABANGA")
  titleTex  <- SDL.createTextureFromSurface renderer titleSurf
  SDL.freeSurface titleSurf
  textureMap %= Map.insert "menu-title" titleTex
  -- Options du menu (version normale et sélectionnée)
  forM_ (zip [0..] menuTexts) $ \ (i, txt) -> do
    surf    <- SDLF.solid font white (T.pack txt)
    tex     <- SDL.createTextureFromSurface renderer surf
    SDL.freeSurface surf
    textureMap %= Map.insert ("menu-" <> show (i :: Int)) tex
    surfSel <- SDLF.solid font yellow (T.pack $ "> " <> txt <> " <")
    texSel  <- SDL.createTextureFromSurface renderer surfSel
    SDL.freeSurface surfSel
    textureMap %= Map.insert ("menu-sel-" <> show i) texSel

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
