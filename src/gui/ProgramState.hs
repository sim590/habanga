
{-|
  Module      : ProgramState
  Description : État global du programme
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  Types d'état global de l'interface graphique SDL.
-}

{-# LANGUAGE TemplateHaskell #-}

module ProgramState
    ( KeyboardState(..)
    , pressedKeys
    , ProgramState(..)
    , sdlRenderer
    , fontMap
    , textureMap
    , keyboardState
    , menuSt
    , initProgramState
    ) where

import Data.Set (Set)
import Data.Default
import Data.Map (Map)

import Control.Lens

import MenuState

import qualified SDL
import SDL.Font (Font)

newtype KeyboardState = KeyboardState { _pressedKeys :: Set SDL.Keycode
                                      }
makeLenses ''KeyboardState

instance Default KeyboardState where
  def = KeyboardState mempty

data ProgramState = ProgramState { _sdlRenderer   :: SDL.Renderer
                                 , _fontMap       :: Map String Font
                                 , _textureMap    :: Map String SDL.Texture
                                 , _keyboardState :: KeyboardState
                                 , _menuSt        :: MenuState
                                 }

makeLenses ''ProgramState

-- | Créer l'état initial du programme
initProgramState :: SDL.Renderer -> ProgramState
initProgramState renderer = ProgramState renderer mempty mempty def def

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
