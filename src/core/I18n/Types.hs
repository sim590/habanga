module I18n.Types where

import Cards (Color (..))

data Translations = Translations
    { btnPlay           :: String
    , btnOnlinePlay     :: String
    , btnOptions        :: String
    , btnQuit           :: String
    , btnCreateGame     :: String
    , btnJoinGame       :: String
    , btnCancel         :: String
    , lblPlayerNames    :: String
    , lblYourName       :: String
    , lblNumPlayers     :: String
    , lblGameCode       :: String
    , titleMainMenu     :: String
    , titleGameConfig   :: String
    , titleOnlineConfig :: String
    , titleOnlineGame   :: String
    , titleGameOver     :: String
    , titleKeyBindings  :: String
    , titleGameLog      :: String
    , msgValidate       :: String
    , msgPleaseWait     :: String
    , msgWon            :: String
    , msgWaitingConnect :: String
    , msgGameCodeLabel  :: String
    , kbLeftRight       :: String
    , kbSelectCard      :: String
    , kbPlayLeft        :: String
    , kbPlayRight       :: String
    , kbQuit            :: String
    , colorRed          :: String
    , colorYellow       :: String
    , colorBlue         :: String
    , colorPurple       :: String
    , colorGrey         :: String
    , labelPlayer       :: String
    , labelTurn         :: String
    , gameLogTitle      :: String
    , msgWaitingPlayers :: Int -> Int -> String
    , logDrewCards      :: Int -> String
    , logPlayedCard     :: String -> String -> String -> String
    , playersTurn       :: String -> String 
    }

displayColor :: Translations -> Maybe Color -> String
displayColor t (Just Red)    = colorRed t 
displayColor t (Just Yellow) = colorYellow t
displayColor t (Just Blue)   = colorBlue t
displayColor t (Just Purple) = colorPurple t
displayColor t Nothing       = colorGrey t
