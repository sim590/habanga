{-# LANGUAGE OverloadedStrings #-}

module GUI.MenuState
    ( MenuState(..)
    , menuActions
    , optionsAction
    , continueAction
    , menuIndex
    , isMenuOpen
    , changeMenuOption
    , closeMenu
    , openMenu
    ) where

import Data.Set (Set)
import qualified Data.Set as S
import SDL
import SDL.Font (Font)

import Data.Default
import qualified Data.Map.Strict as Map
import Control.Monad.State

-- | État du menu
data MenuState = MenuState 
  { _menuOption    :: Int              -- ^ Index de l'option sélectionnée
  , _menuOpen      :: Bool             -- ^ Le menu est-il visible ?
  } deriving (Show, Eq)

instance Default MenuState where
  def = MenuState 0 True

-- Liste des options du menu en ordre
menuTexts :: [String]
menuTexts = ["New Game", "Continue", "Options", "Quit"]

-- Mapping actions par index
data MenuAction = NewGame | Continue | Options | QuitGame deriving (Show, Eq)
menuActions :: Int -> MenuAction
menuActions 0 = NewGame
menuActions 1 = Continue
menuActions 2 = Options
menuActions _ = QuitGame

-- State du menu
type MenuStateState = State MenuState

-- Accès à l'index de l'option sélectionnée
menuIndex :: MenuStateState MenuState
menuIndex = gets _menuOption

-- Vérifier si le menu est ouvert
isMenuOpen :: MenuStateState MenuState
isMenuOpen = gets _menuOpen

-- Changer l'option sélectionnée (avec wrap-around)
changeMenuOption :: Int -> MenuStateState MenuState
changeMenuOption delta = do
  current <- gets _menuOption
  let newOption = ((current + delta + length menuTexts) `mod` length menuTexts)
  put $ MenuState { _menuOption = newOption, _menuOpen = True }

-- Fermer le menu
closeMenu :: MenuStateState MenuState
closeMenu = do
  put $ MenuState { _menuOption = 0, _menuOpen = False }

-- Ouvrir le menu (réinitialise à la première option)
openMenu :: MenuState -> IO ()
openMenu = do
  put $ MenuState { _menuOption = 0, _menuOpen = True }
