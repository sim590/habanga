
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

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.FileEmbed (embedFile, makeRelativeToProject)

import Foreign.C.Types (CInt)
import Foreign.Ptr (castPtr)
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Extra ( whileM )
import Control.Concurrent
import Control.Monad.State
import Control.Lens

import MenuState

import System.Environment

import qualified SDL
import qualified SDL.Raw
import qualified SDL.Raw.Image as SDLRI
import SDL.Internal.Types (Renderer(..))
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
                                 , _menuSt        :: MenuState
                                 }

makeLenses ''ProgramState

-- | Créer l'état initial du programme
initProgramState :: SDL.Renderer -> ProgramState
initProgramState renderer = ProgramState renderer mempty mempty def def

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

-- | Image de fond du menu, imbriquée dans le binaire à la compilation
bgImageData :: BS.ByteString
bgImageData = $(makeRelativeToProject "resources/habanga-gui/fond-ecran.png" >>= embedFile)

-- | Charger une texture PNG depuis un ByteString imbriqué
loadEmbeddedPNG :: SDL.Renderer -> BS.ByteString -> IO SDL.Texture
loadEmbeddedPNG (Renderer rPtr) bs =
  BSU.unsafeUseAsCStringLen bs $ \ (ptr, len) -> do
    rwops   <- SDL.Raw.rwFromConstMem (castPtr ptr) (fromIntegral len)
    surfPtr <- SDLRI.loadPNG_RW rwops
    tex     <- SDL.Raw.createTextureFromSurface rPtr surfPtr
    SDL.Raw.freeSurface surfPtr
    return (unsafeCoerce tex :: SDL.Texture)

-- TODO: les textures devraient sûrement être placées dans un module à part.
-- Toutes ces textures seront ensuite chargées dans l'état du programme.
loadTextures :: StateT ProgramState IO ()
loadTextures = do
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

loadFonts :: StateT ProgramState IO ()
loadFonts = do
  homeDir <- liftIO $ getEnv "HOME"
  font    <- SDLF.load (homeDir <> "/.local/share/fonts/mighty-souly-font/MightySouly-lxggD.ttf") 64
  fontMap %= Map.insert "default" font

loadData :: StateT ProgramState IO ()
loadData = loadFonts >> loadTextures

-- | Gestion de l'input du menu à partir d'un événement SDL
handleMenuEvent :: SDL.EventPayload -> StateT ProgramState IO ()
handleMenuEvent (SDL.KeyboardEvent ke)
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
          handleMenuAction (menuActions idx)
        SDL.KeycodeReturn -> do
          idx <- use (menuSt . menuOption)
          handleMenuAction (menuActions idx)
        _                 -> return ()
  | otherwise = return ()
handleMenuEvent (SDL.MouseButtonEvent mbe)
  | SDL.mouseButtonEventMotion mbe == SDL.Pressed
  , SDL.mouseButtonEventButton mbe == SDL.ButtonLeft = do
      isOpen <- use (menuSt . menuOpen)
      -- TODO: implémenter la détection du clic sur un item du menu
      --       (nécessite de connaître les coordonnées de chaque item)
      when isOpen $ return ()
handleMenuEvent _ = return ()

-- | Exécuter l'action correspondant à l'option du menu sélectionnée
handleMenuAction :: MenuAction -> StateT ProgramState IO ()
handleMenuAction NewGame  = menuSt . menuOpen .= False -- TODO: démarrer une nouvelle partie
handleMenuAction Continue = menuSt . menuOpen .= False -- TODO: continuer la partie en cours
handleMenuAction Options  = return ()                  -- TODO: ouvrir le sous-menu d'options
handleMenuAction QuitGame = menuSt . menuOpen .= False -- Sera capté par continueLoop

handleEventPayload :: SDL.EventPayload -> StateT ProgramState IO ()
handleEventPayload payload = do
  isOpen <- use (menuSt . menuOpen)
  if isOpen
    then handleMenuEvent payload
    else do
      -- Quand le menu est fermé, gérer les inputs de gameplay
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
  isOpen <- use (menuSt . menuOpen)
  if isOpen then return True  -- Le menu est ouvert, on continue la boucle
            else do
              keys <- use (keyboardState . pressedKeys)
              idx  <- use (menuSt . menuOption)
              -- Quitter si Q est pressé ou si QuitGame a été sélectionné
              return $ not (Set.member SDL.KeycodeQ keys) && menuActions idx /= QuitGame

-- | Dessiner une texture centrée horizontalement à une position Y donnée
drawCentered :: SDL.Renderer -> SDL.Texture -> CInt -> CInt -> StateT ProgramState IO ()
drawCentered renderer tex screenW y = do
  info <- SDL.queryTexture tex
  let w = SDL.textureWidth info
      h = SDL.textureHeight info
      x = (screenW - w) `div` 2
  SDL.copy renderer tex Nothing (Just $ SDL.Rectangle (SDL.P (SDL.V2 x y)) (SDL.V2 w h))

-- | Rendu du menu
renderMenu :: StateT ProgramState IO ()
renderMenu = do
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
  forM_ (zip [0..] menuTexts) $ \(i, _) -> do
    let key = if i == selectedIdx
              then "menu-sel-" <> show i
              else "menu-"     <> show i
        y   = 300 + fromIntegral i * 80
    case Map.lookup key textures of
      Just tex -> drawCentered renderer tex 1280 y
      Nothing  -> return ()

loop :: StateT ProgramState IO ()
loop = whileM $ do
  renderer <- use sdlRenderer

  events <- SDL.pollEvents
  forM_ events (handleEventPayload . SDL.eventPayload)

  isOpen <- use (menuSt . menuOpen)

  if isOpen
    then renderMenu
    else do
      -- Rendu gameplay normal
      SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 255
      SDL.clear renderer
      -- TODO: rendu du gameplay

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
