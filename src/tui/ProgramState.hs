
{-|
  Module      : ProgramState
  Description : Définitions de l'état de l'interface texte
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module ProgramState where

import qualified Data.Text as T
import Data.Default
import Data.FileEmbed

import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM

import Brick.BChan
import Brick.Forms
import Brick.Focus

import GameState
import NetworkState ( NetworkState
                    , NetworkRequest
                    )
import qualified BrickNetworkBridge as BNB

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
  def = ProgramResources $(makeRelativeToProject "resources/habanga-tui/Habanga-title.txt"            >>= embedStringFile)
                         $(makeRelativeToProject "resources/habanga-tui/habanga-bluecard-35x53.txt"   >>= embedStringFile)
                         $(makeRelativeToProject "resources/habanga-tui/habanga-redcard-35x53.txt"    >>= embedStringFile)
                         $(makeRelativeToProject "resources/habanga-tui/habanga-yellowcard-35x53.txt" >>= embedStringFile)
                         $(makeRelativeToProject "resources/habanga-tui/habanga-purplecard-35x53.txt" >>= embedStringFile)
                         $(makeRelativeToProject "resources/habanga-tui/habanga-bluecard-10x15.txt"   >>= embedStringFile)
                         $(makeRelativeToProject "resources/habanga-tui/habanga-redcard-10x15.txt"    >>= embedStringFile)
                         $(makeRelativeToProject "resources/habanga-tui/habanga-yellowcard-10x15.txt" >>= embedStringFile)
                         $(makeRelativeToProject "resources/habanga-tui/habanga-purplecard-10x15.txt" >>= embedStringFile)

data GameInitializationFormElement = GameInitializationFormPlayerNamesField
  deriving (Eq, Ord, Show, Enum)

data OnlineGameInitializationFormElement = OnlineGameInitializationFormMyNameField
                                         | OnlineGameInitializationFormNumberOfPlayersField
                                         | OnlineGameInitializationFormGameCodeField
  deriving (Eq, Ord, Show, Enum)

data MainMenuFocusableElement = MainMenuButtons
                              | GameInitializationForm GameInitializationFormElement
                              | OnlineGameInitializationForm OnlineGameInitializationFormElement
                              | OnlineLobbyFocus
                              deriving (Eq, Ord, Show)

data GameFocusableSubElement = GameLog
  deriving (Eq, Ord, Show)

data AppFocus = MainMenu MainMenuFocusableElement
              | OnlineGameMenu
              | OptionsMenu
              | Game (Maybe GameFocusableSubElement)
              deriving (Eq, Ord, Show)

newtype GameInitializationInfo = GameInitializationInfo { _playerNamesField :: T.Text }
  deriving Show
makeLenses ''GameInitializationInfo

instance Default GameInitializationInfo where
  def = GameInitializationInfo mempty

data OnlineGameInitializationInfo = OnlineGameInitializationInfo { _myPlayerName    :: T.Text
                                                                 , _numberOfPlayers :: Int
                                                                 , _gameCode        :: T.Text
                                                                 }
  deriving Show
makeLenses ''OnlineGameInitializationInfo

instance Default OnlineGameInitializationInfo where
  def = OnlineGameInitializationInfo mempty 0 mempty

data OnlineRole = Host
                | OtherPlayer

data MainMenuSubMenu = GameInitialization { _gameForm :: Form GameInitializationInfo BNB.NetworkBrickEvent AppFocus
                                          }
                     | OnlineGameSubMenu { _onlineGameMenuIndex :: Int }
                     | OnlineGameInitialization { _onlineGameForm :: Form OnlineGameInitializationInfo BNB.NetworkBrickEvent AppFocus
                                                , _playerRole     :: OnlineRole
                                                }
                     | OnlineLobby OnlineRole
makeLenses ''MainMenuSubMenu

data MainMenuState = MainMenuState { _mainMenuIndex :: Int
                                   , _submenu       :: Maybe MainMenuSubMenu
                                   }
makeLenses ''MainMenuState

instance Default MainMenuState where
  def = MainMenuState 0 Nothing

data GameViewState = GameViewState { _gameViewIndex :: Int
                                   , _winner        :: Maybe String
                                   , _gameLog       :: [String]
                                   }
makeLenses ''GameViewState

instance Default GameViewState where
  def = GameViewState 0 Nothing []

data ProgramState = ProgramState { _gameState             :: GameState
                                 , _networkState          :: NetworkState
                                 , _programResources      :: ProgramResources
                                 , _mainMenuState         :: MainMenuState
                                 , _gameViewState         :: GameViewState
                                 , _currentFocus          :: FocusRing AppFocus
                                 , _networkRequestChannel :: Maybe (TChan NetworkRequest)
                                 , _brickEventChannel     :: Maybe (BChan BNB.NetworkBrickEvent)
                                 , _networkMV             :: Maybe (MVar ())
                                 , _brickNetworkBridgeMV  :: Maybe (MVar ())
                                 }
makeLenses ''ProgramState

instance Default ProgramState where
  def = ProgramState { _gameState             = def
                     , _networkState          = def
                     , _programResources      = def
                     , _mainMenuState         = def
                     , _gameViewState         = def
                     , _currentFocus          = focusRing [ MainMenu MainMenuButtons
                                                         , MainMenu (GameInitializationForm GameInitializationFormPlayerNamesField)
                                                         , MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormMyNameField)
                                                         , MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormNumberOfPlayersField)
                                                         , MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormGameCodeField)
                                                         , MainMenu OnlineLobbyFocus
                                                         , OptionsMenu
                                                         , Game Nothing
                                                         ]
                     , _networkRequestChannel = Nothing
                     , _brickEventChannel     = Nothing
                     , _networkMV             = Nothing
                     , _brickNetworkBridgeMV  = Nothing
                     }

instance GameStated ProgramState where
  getGameState       = _gameState
  setGameState ps gs = ps { _gameState = gs }

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

