
{-# LANGUAGE TemplateHaskell #-}

module ProgramState where

import qualified Data.Text as T
import Data.Default

import Control.Lens

import Brick.Forms

import GameState

_HABANGA_MENU_GAMETITLE_FILE_PATH_ :: String
_HABANGA_MENU_GAMETITLE_FILE_PATH_ = "resources/habanga-tui/Habanga-title.txt"

_HABANGA_PURPLECARD_35X53_ :: String
_HABANGA_PURPLECARD_35X53_ = "resources/habanga-tui/habanga-purplecard-35x53.txt"

_HABANGA_REDCARD_35X53_ :: String
_HABANGA_REDCARD_35X53_ = "resources/habanga-tui/habanga-redcard-35x53.txt"

_HABANGA_YELLOWCARD_35X53_ :: String
_HABANGA_YELLOWCARD_35X53_ = "resources/habanga-tui/habanga-yellowcard-35x53.txt"

_HABANGA_BLUECARD_35X53_ :: String
_HABANGA_BLUECARD_35X53_ = "resources/habanga-tui/habanga-bluecard-35x53.txt"

_HABANGA_PURPLECARD_10X15 :: String
_HABANGA_PURPLECARD_10X15 = "resources/habanga-tui/habanga-purplecard-10x15.txt"

_HABANGA_YELLOWCARD_10X15 :: String
_HABANGA_YELLOWCARD_10X15 = "resources/habanga-tui/habanga-yellowcard-10x15.txt"

_HABANGA_REDCARD_10X15    :: String
_HABANGA_REDCARD_10X15 = "resources/habanga-tui/habanga-redcard-10x15.txt"

_HABANGA_BLUECARD_10X15   :: String
_HABANGA_BLUECARD_10X15 = "resources/habanga-tui/habanga-bluecard-10x15.txt"

_HABANGA_MAX_PLAYER_COUNT_ :: Int
_HABANGA_MAX_PLAYER_COUNT_ = 6

data ProgramResources = ProgramResources { _menuGameTitle   :: String
                                         , _blueCard35x53   :: String
                                         , _redCard35x53    :: String
                                         , _yellowCard35x53 :: String
                                         , _purpleCard35x53 :: String
                                         , _blueCard10x15   :: String
                                         , _redCard10x15    :: String
                                         , _yellowCard10x15 :: String
                                         , _purpleCard10x15 :: String
                                         }
makeLenses ''ProgramResources

instance Default ProgramResources where
  def = ProgramResources "[menuGameTitleMissing]"
                         "[blueCard35x53Missing]"
                         "[redCard35x53Missing]"
                         "[yellowCard35x53Missing]"
                         "[purpleCard35x53Missing]"
                         "[blueCard10x15Missing]"
                         "[redCard10x15Missing]"
                         "[yellowCard10x15Missing]"
                         "[purpleCard10x15Missing]"

data AppFocus = GameInitializationFormPlayerNamesField
  deriving (Eq, Ord, Show)

newtype GameInitializationInfo = GameInitializationInfo { _playerNamesField :: T.Text }
  deriving Show
makeLenses ''GameInitializationInfo

instance Default GameInitializationInfo where
  def = GameInitializationInfo mempty

newtype MainMenuSubMenu = GameInitialization { _gameForm :: Form GameInitializationInfo () AppFocus
                                             }
makeLenses ''MainMenuSubMenu

data MainMenuState = MainMenuState { _mainMenuIndex :: Int
                                   , _submenu       :: Maybe MainMenuSubMenu
                                   }
makeLenses ''MainMenuState

instance Default MainMenuState where
  def = MainMenuState 0 Nothing

newtype GameViewState = GameViewState { _gameViewIndex :: Int }
makeLenses ''GameViewState

instance Default GameViewState where
  def = GameViewState 0

data Screen = MainMenu | OptionsMenu | Game
  deriving Show

data ProgramState = ProgramState { _gameState        :: GameState
                                 , _programResources :: ProgramResources
                                 , _mainMenuState    :: MainMenuState
                                 , _gameViewState    :: GameViewState
                                 , _currentScreen    :: Maybe Screen
                                 }
makeLenses ''ProgramState

instance Default ProgramState where
  def = ProgramState def def def def def

instance GameStated ProgramState where
  getGameState       = _gameState
  setGameState ps gs = ps { _gameState = gs }

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

