module I18n.En where

import I18n.Types

enTranslations :: Translations
enTranslations = Translations { _mainMenuTranslations = _mainMenuTranslations
                              , _gameViewTranslations = _gameViewTranslations
                              }
    where
        _mainMenuTranslations = MainMenuTranslations { _btnPlay           = "Play"
                                                     , _btnOnlinePlay     = "Play Online"
                                                     , _btnOptions        = "Options"
                                                     , _btnQuit           = "Quit"
                                                     , _btnCreateGame     = "Create a game"
                                                     , _btnJoinGame       = "Join a game"
                                                     , _btnCancel         = "Cancel"
                                                     , _lblPlayerNames    = "Player names"
                                                     , _lblYourName       = "Your name"
                                                     , _lblNumPlayers     = "Number of players (2-6)"
                                                     , _lblGameCode       = "Game code"
                                                     , _titleMainMenu     = "Main menu"
                                                     , _titleGameConfig   = "Game configuration"
                                                     , _titleOnlineConfig = "Game configuration (multiplayer)"
                                                     , _titleOnlineGame   = "Online game"
                                                     , _titleGameOver     = "Game over!"
                                                     , _titleKeyBindings  = "Controls"
                                                     , _titleGameLog      = "Game log"
                                                     }
        _gameViewTranslations = GameViewTranslations { _msgValidate       = "Ctrl-g to confirm."
                                                     , _msgPleaseWait     = "Please wait..."
                                                     , _msgWon            = " won!"
                                                     , _msgWaitingConnect = "Waiting for connection"
                                                     , _msgGameCodeLabel  = "Game code: "
                                                     , _kbLeftRight       = "left/right"
                                                     , _kbSelectCard      = "Select a card"
                                                     , _kbPlayLeft        = "Play left"
                                                     , _kbPlayRight       = "Play right"
                                                     , _kbQuit            = "Quit"
                                                     , _colorRed          = "Red"
                                                     , _colorYellow       = "Yellow"
                                                     , _colorBlue         = "Blue"
                                                     , _colorPurple       = "Purple"
                                                     , _colorGrey         = "Grey"
                                                     , _labelPlayer       = "Player: "
                                                     , _labelTurn         = ", Turn: "
                                                     , _gameLogTitle      = "Game Log "
                                                     , _msgWaitingPlayers = \c e -> "Waiting for players (" <> show c <> "/" <> show e <> ")"
                                                     , _logDrewCards      = \n -> ">> drew " <> show n <> " card(s)!"
                                                     , _logPlayedCard     = \pname val col -> "Player " <> pname <> ":\n>> played " <> val <> " " <> col <> "."
                                                     , _playersTurn       = \pname -> pname <> "'s Turn"
                                                     }
