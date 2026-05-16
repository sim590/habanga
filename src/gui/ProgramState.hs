
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
    , ProgramResources(..)
    , bgImageData
    , programResources
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
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile, makeRelativeToProject)

import Control.Lens

import MenuState

import qualified SDL
import SDL.Font (Font)

newtype KeyboardState = KeyboardState { _pressedKeys :: Set SDL.Keycode
                                       }
makeLenses ''KeyboardState

instance Default KeyboardState where
  def = KeyboardState mempty

data ProgramResources = ProgramResources { _bgImageData :: ByteString
                                         }
makeLenses ''ProgramResources

instance Default ProgramResources where
  def = ProgramResources
    $(makeRelativeToProject "resources/habanga-gui/fond-ecran.png" >>= embedFile)

data ProgramState = ProgramState { _sdlRenderer      :: Maybe SDL.Renderer
                                 , _fontMap          :: Map String Font
                                 , _textureMap       :: Map String SDL.Texture
                                 , _keyboardState    :: KeyboardState
                                 , _menuSt           :: MenuState
                                 , _programResources :: ProgramResources
                                 }

makeLenses ''ProgramState

instance Default ProgramState where
  def = ProgramState { _sdlRenderer      = Nothing
                     , _fontMap          = mempty
                     , _textureMap       = mempty
                     , _keyboardState    = def
                     , _menuSt           = def
                     , _programResources = def
                     }

-- | Créer l'état initial du programme
initProgramState :: SDL.Renderer -> ProgramState
initProgramState renderer = def & sdlRenderer .~ Just renderer

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
