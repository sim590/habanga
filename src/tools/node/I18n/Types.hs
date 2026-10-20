{-# LANGUAGE TemplateHaskell #-}

module I18n.Types where

import Control.Lens

data Translations = Translations { _errInvalidParameter         :: String
                                 , _errInvalidPlayerCount       :: String -> String
                                 , _msgDrawCards                :: Int -> String
                                 , _errCannotPlayCard           :: String
                                 , _errNoTurnToProcess          :: String
                                 , _errUnkownCommand            :: String
                                 , _titleCommands               :: String
                                 , _titleKeyBindings            :: String
                                 , _lblPrompt                   :: String
                                 , _helpUsageHelp               :: String
                                 , _helpDescHelp                :: String
                                 , _helpUsageAnnounceGameShort  :: String
                                 , _helpUsageAnnounceGame       :: String
                                 , _helpDescAnnounceGame        :: String
                                 , _helpUsageJoinGameShort      :: String
                                 , _helpUsageJoinGame           :: String
                                 , _helpDescJoinGame            :: String
                                 , _helpDescStartGame           :: String
                                 , _helpUsagePlayTurn           :: String
                                 , _helpDescPlayTurn            :: String
                                 , _helpDescProcessOtherTurn    :: String
                                 , _helpDescResetNetwork        :: String
                                 , _helpDescPrintGameState      :: String
                                 , _helpDescClearLog            :: String
                                 , _helpDescQuit                :: String
                                 , _kbHelp                      :: String
                                 , _kbClearLog                  :: String
                                 , _kbCloseWindow               :: String
                                 }

makeLenses ''Translations

