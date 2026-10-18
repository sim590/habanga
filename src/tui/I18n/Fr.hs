module I18n.Fr where

import I18n.Types

frTranslations :: Translations
frTranslations = Translations { _mainMenuTranslations = _mainMenuTranslations
                                 , _gameViewTranslations = _gameViewTranslations
                                 }
    where
        _mainMenuTranslations = MainMenuTranslations { _btnPlay           = "Jouer"
                                                     , _btnOnlinePlay     = "Jouer en ligne"
                                                     , _btnOptions        = "Options"
                                                     , _btnQuit           = "Quitter"
                                                     , _btnCreateGame     = "Créer une partie"
                                                     , _btnJoinGame       = "Joindre une partie"
                                                     , _btnCancel         = "Annuler"
                                                     , _lblPlayerNames    = "Nom des joueurs"
                                                     , _lblYourName       = "Votre nom"
                                                     , _lblNumPlayers     = "Nombre de joueurs (2-6)"
                                                     , _lblGameCode       = "Le code de la partie"
                                                     , _titleMainMenu     = "Menu principal"
                                                     , _titleGameConfig   = "Configuration du jeu"
                                                     , _titleOnlineConfig = "Configuration du jeu (multijoueur)"
                                                     , _titleOnlineGame   = "Jeu en ligne"
                                                     , _titleGameOver     = "Fin de partie!"
                                                     , _titleKeyBindings  = "Touches"
                                                     , _titleGameLog      = "Journal du jeu"
                                                     }
        _gameViewTranslations = GameViewTranslations { _msgValidate       = "Ctrl-g pour valider."
                                                     , _msgPleaseWait     = "Veuillez patientez..."
                                                     , _msgWon            = " a gagné!"
                                                     , _msgWaitingConnect = "En attente de connexion"
                                                     , _msgGameCodeLabel  = "Code de la partie: "
                                                     , _kbLeftRight       = "gauche/droite"
                                                     , _kbSelectCard      = "Sélectionner une carte"
                                                     , _kbPlayLeft        = "Jouer à gauche"
                                                     , _kbPlayRight       = "Jouer à droite"
                                                     , _kbQuit            = "Quitter"
                                                     , _colorRed          = "Rouge"
                                                     , _colorYellow       = "Jaune"
                                                     , _colorBlue         = "Bleu"
                                                     , _colorPurple       = "Mauve"
                                                     , _colorGrey         = "Gris"
                                                     , _labelPlayer       = "Joueur: "
                                                     , _labelTurn         = ", Tour: "
                                                     , _gameLogTitle      = "Journal de Jeu "
                                                     , _msgWaitingPlayers = \c e -> "En attente des joueurs (" <> show c <> "/" <> show e <> ")"
                                                     , _logDrewCards      = \n -> ">> pige " <> show n <> " carte(s)!"
                                                     , _logPlayedCard     = \pname val col -> "Joueur " <> pname <> ":\n>> a joué le " <> val <> " " <> col <> "."
                                                     , _playersTurn       = \pname -> "Tour de " <> pname
                                                     }
