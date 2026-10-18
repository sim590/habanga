module I18n.Fr where
   
import I18n.Types
   
frTranslations :: Translations
frTranslations = Translations
        { _errInvalidParameter         = "err.. Un des paramètres est invalide!"
                                     , _errInvalidPlayerCount       = \n -> "err.. Le nombre de joueurs '" <> n <> "' n'a pas pu être résolu à un entier!"
                                     , _msgDrawCards                = \n -> "Vous pigez " <> show n <> " carte(s)!"
                                     , _errCannotPlayCard           = "err.. Impossible de jouer cette carte!"
                                     , _errNoTurnToProcess          = "err.. Aucun tour à traiter!"
                                     , _errUnkownCommand            = "err.. impossible d'exécuter cette commande!"
                                     , _titleCommands               = "COMMANDES"
                                     , _titleKeyBindings            = "TOUCHES"
                                     , _lblPrompt                   = "node> "
                                     , _helpUsageHelp               = "aide"
                                     , _helpDescHelp                = "  Affiche l'aide de cet utilitaire."
                                     , _helpUsageAnnounceGameShort  = "ag {n} {code} {nomHôte}"
                                     , _helpUsageAnnounceGame       = "announceGame {n} {code} {nomHôte}"
                                     , _helpDescAnnounceGame        = "  Annonce une partie à {n} joueurs sur le réseau\n\ 
                                                                     \  avec un code {code} et un nom de joueur de\n\ 
                                                                     \  l'hôte {nomHôte}."
                                     , _helpUsageJoinGameShort      = "rj {code} {nomJoueur}"
                                     , _helpUsageJoinGame           = "requestJoinGame {code} {nomJoueur}"
                                     , _helpDescJoinGame            = "  Demande d'accéder à la partie en tant que {nomJoueur}\n\ 
                                                                     \  en utilisant le code {code}."
                                     , _helpDescStartGame           = "  Initialise l'état du jeu et prépare la couche réseau."
                                     , _helpUsagePlayTurn           = "playTurn {numéro} {couleur} {emplacement}"
                                     , _helpDescPlayTurn            = "  Jouer à son tour une carte de numéro {numéro} et de couleur\n\ 
                                                                     \  {couleur} à l'emplacement {emplacement} (gauche/droite)."
                                     , _helpDescProcessOtherTurn    = "  Joue le tour d'un autre joueur. Les tours sont accumulés\n\ 
                                                                     \  en cache. Cette commande doit être exécutée pour chaque tour\n\ 
                                                                     \  des autres joueurs. Celle-ci n'aura un effet que si le tour\n\ 
                                                                     \  du joueur a déjà été reçu."
                                     , _helpDescResetNetwork        = "  Réinitialisation du réseau et de l'état du jeu."
                                     , _helpDescPrintGameState      = "  Affiche l'état actuel du jeu."
                                     , _helpDescClearLog            = "  Efface le contenu du journal."
                                     , _helpDescQuit                = "  Quitter le programme."
                                     , _kbHelp                      = "?:      Affiche l'aide de cet utilitaire."
                                     , _kbClearLog                  = "CTRL-l: Efface le contenu du journal."
                                     , _kbCloseWindow               = "ESC, q: Quitter la fenêtre."
                                     }
