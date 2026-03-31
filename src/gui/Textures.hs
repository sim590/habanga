
{-|
  Module      : Textures
  Description : Utilitaires de chargement de textures
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  Fonctions utilitaires pour charger des textures SDL depuis des données
  imbriquées dans le binaire.
-}

module Textures
    ( loadEmbeddedPNG
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Foreign.Ptr (castPtr)
import Unsafe.Coerce (unsafeCoerce)

import qualified SDL
import qualified SDL.Raw
import qualified SDL.Raw.Image as SDLRI
import SDL.Internal.Types (Renderer(..))

-- | Charger une texture PNG depuis un ByteString imbriqué dans le binaire.
--
--   Utilise SDL.Raw pour contourner l'absence du constructeur SDL.Texture
--   dans l'API publique de sdl2. Le unsafeCoerce est sûr car SDL.Texture
--   est un simple newtype autour de Ptr TextureStruct.
loadEmbeddedPNG :: SDL.Renderer -> BS.ByteString -> IO SDL.Texture
loadEmbeddedPNG (Renderer rPtr) bs =
  BSU.unsafeUseAsCStringLen bs $ \ (ptr, len) -> do
    rwops   <- SDL.Raw.rwFromConstMem (castPtr ptr) (fromIntegral len)
    surfPtr <- SDLRI.loadPNG_RW rwops
    tex     <- SDL.Raw.createTextureFromSurface rPtr surfPtr
    SDL.Raw.freeSurface surfPtr
    return (unsafeCoerce tex :: SDL.Texture)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :
