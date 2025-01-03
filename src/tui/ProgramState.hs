
{-# LANGUAGE TemplateHaskell #-}

module ProgramState where

import Data.Default
import Control.Lens

import Game

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

data ProgramResources = ProgramResources { _menuGameTitle   :: String
                                         , _blueCard35x53   :: String
                                         , _redCard35x53    :: String
                                         , _yellowCard35x53 :: String
                                         , _purpleCard35x53 :: String
                                         }
makeLenses ''ProgramResources

instance Default ProgramResources where
  def = ProgramResources "[menuGameTitleMissing]"
                         "[blueCard35x53Missing]"
                         "[redCard35x53Missing]"
                         "[yellowCard35x53Missing]"
                         "[purpleCard35x53Missing]"

newtype MainMenuState = MainMenuState { _menuIndex :: Int }
makeLenses ''MainMenuState

instance Default MainMenuState where
  def = MainMenuState 0

data Screen = MainMenu | OptionsMenu | Game

data ProgramState = ProgramState { _gameState     :: GameState
                                 , _gameResources :: ProgramResources
                                 , _mainMenuState :: MainMenuState
                                 , _currentScreen :: Maybe Screen
                                 }
makeLenses ''ProgramState

instance Default ProgramState where
  def = ProgramState def def def def

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

