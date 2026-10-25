{-# LANGUAGE TemplateHaskell #-}

module I18n.Types where

import Control.Lens

import Cards (Color (..))

data MainMenuTranslations = MainMenuTranslations { _btnPlay           :: String
                                                 , _btnOnlinePlay     :: String
                                                 , _btnOptions        :: String
                                                 , _btnQuit           :: String
                                                 , _btnCreateGame     :: String
                                                 , _btnJoinGame       :: String
                                                 , _btnCancel         :: String
                                                 , _lblPlayerNames    :: String
                                                 , _lblYourName       :: String
                                                 , _lblNumPlayers     :: String
                                                 , _lblGameCode       :: String
                                                 , _titleMainMenu     :: String
                                                 , _titleGameConfig   :: String
                                                 , _titleOnlineConfig :: String
                                                 , _titleOnlineGame   :: String
                                                 , _titleGameOver     :: String
                                                 , _titleKeyBindings  :: String
                                                 , _titleGameLog      :: String
                                                 }
makeLenses ''MainMenuTranslations

data GameViewTranslations = GameViewTranslations { _msgValidate       :: String
                                                 , _msgPleaseWait     :: String
                                                 , _msgWon            :: String
                                                 , _msgWaitingConnect :: String
                                                 , _msgGameCodeLabel  :: String
                                                 , _kbLeftRight       :: String
                                                 , _kbSelectCard      :: String
                                                 , _kbPlayLeft        :: String
                                                 , _kbPlayRight       :: String
                                                 , _kbQuit            :: String
                                                 , _colorRed          :: String
                                                 , _colorYellow       :: String
                                                 , _colorBlue         :: String
                                                 , _colorPurple       :: String
                                                 , _colorGrey         :: String
                                                 , _labelPlayer       :: String
                                                 , _labelTurn         :: String
                                                 , _gameLogTitle      :: String
                                                 , _msgWaitingPlayers :: Int -> Int -> String
                                                 , _logDrewCards      :: Int -> String
                                                 , _logPlayedCard     :: String -> String -> String -> String
                                                 , _playersTurn       :: String -> String
                                                 }
makeLenses ''GameViewTranslations

data Translations = Translations { _mainMenuTranslations :: MainMenuTranslations
                                 , _gameViewTranslations :: GameViewTranslations
                                 }
makeLenses ''Translations


displayColor :: GameViewTranslations -> Maybe Color -> String
displayColor t (Just Red)    = t ^. colorRed 
displayColor t (Just Yellow) = t ^. colorYellow
displayColor t (Just Blue)   = t ^. colorBlue
displayColor t (Just Purple) = t ^. colorPurple
displayColor t Nothing       = t ^. colorGrey
