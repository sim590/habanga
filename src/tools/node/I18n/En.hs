module I18n.En where

import I18n.Types

enTranslations :: Translations
enTranslations = Translations        { _errInvalidParameter         = "err.. One of the parameters is invalid!"
                                     , _errInvalidPlayerCount       = \n -> "err.. The number of players '" <> n <> "' could not be parsed as an integer!"
                                     , _msgDrawCards                = \n -> "You draw " <> show n <> " card(s)!"
                                     , _errCannotPlayCard           = "err.. Unable to play this card!"
                                     , _errNoTurnToProcess          = "err.. No turn to process!"
                                     , _errUnkownCommand            = "err.. unable to execute this command!"
                                     , _titleCommands               = "COMMANDS"
                                     , _titleKeyBindings            = "KEYS"
                                     , _lblPrompt                   = "node> "
                                     , _helpUsageHelp               = "help"
                                     , _helpDescHelp                = " Shows help for this utility."
                                     , _helpUsageAnnounceGameShort  = "ag {n} {code} {hostname}"
                                     , _helpUsageAnnounceGame       = "announceGame {n} {code} {hostname}"
                                     , _helpDescAnnounceGame        = " Announces a game for {n} players on the network\n\ 
                                                                      \ with code {code} and the host's player name\n\ 
                                                                      \ {hostName}."
                                     , _helpUsageJoinGameShort      = "rj {code} {playerName}"
                                     , _helpUsageJoinGame           = "requestJoinGame {code} {playerName}"
                                     , _helpDescJoinGame            = " Requests to join the game as {playerName}\n\ 
                                                                      \ using the code {code}."
                                     , _helpDescStartGame           = " Initializes the game state and prepares the network layer."
                                     , _helpUsagePlayTurn           = "playTurn {number} {color} {slot}"
                                     , _helpDescPlayTurn            = " On your turn, play a card with number {number} and color\n\ 
                                                                      \ {color} in slot {slot} (left/right)."
                                     , _helpDescProcessOtherTurn    = " Play another player's turn. Turns are accumulated\n\ 
                                                                      \ in a cache. This command must be run  for each turn\n\ 
                                                                      \ of the other players. It only has an effect if that\n\ 
                                                                      \ player's turn has already been received"
                                     , _helpDescResetNetwork        = "  Resets the network and the game state."
                                     , _helpDescPrintGameState      = "  Displays the current game state."
                                     , _helpDescClearLog            = "  Clears the contents of the log."
                                     , _helpDescQuit                = "  Quit the program."
                                     , _kbHelp                      = "?:      Shows help for this utility."
                                     , _kbClearLog                  = "CTRL-l: Clears the contents of the log."
                                     , _kbCloseWindow               = "ESC, q: Close the window."
                                     }
