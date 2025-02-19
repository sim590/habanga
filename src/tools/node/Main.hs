
{-|
  Module      : Main
  Description : Point d'entrée de l'outil habanga-node
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Default
import Data.Maybe

import Text.Read

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM

import Brick.AttrMap
import Brick.Types ( BrickEvent
                   , EventM
                   , Widget
                   )
import qualified Brick.Types as T
import Brick.Widgets.Core ( viewport
                          , hLimit
                          , vLimit
                          , str
                          , vBox
                          , (<+>)
                          )
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Main as M
import qualified Brick.Focus as F

import Graphics.Vty (defAttr)
import qualified Graphics.Vty as V

import Network
import GameState

data AppFocus = Input
              | Log
              | HelpBox
  deriving (Ord, Show, Eq, Enum, Bounded)
data NodeState = NodeState { _focusRing   :: F.FocusRing AppFocus
                           , _inputEditor :: E.Editor String AppFocus
                           , _logText     :: [String]
                           , _gameStateTV :: TVar GameState
                           }
makeLenses ''NodeState

clearLog :: EventM AppFocus NodeState ()
clearLog = logText .= [ ]

clearEditorInputText :: EventM AppFocus NodeState ()
clearEditorInputText = inputEditor .= E.editor Input Nothing ""

logViewportScroll :: M.ViewportScroll AppFocus
logViewportScroll = M.viewportScroll Log

executeCmd :: EventM AppFocus NodeState ()
executeCmd = do
  ie   <- use inputEditor
  gsTV <- use gameStateTV
  let
    cmdline      = head $ E.getEditContents ie
    cmdlineToks  = words cmdline
    cmd          = head cmdlineToks
    args         = tail cmdlineToks
    announceGame = case args of
      [n, gc, playerName] -> do
        let mn = readMaybe n
        liftIO $ atomically $ modifyTVar gsTV $ \ gs -> gs
          & networkStatus                .~ Request GameAnnounce
          & myName                       .~ playerName
          -- TODO: ne pas utiliser gameSettings, mais passer directement gamecode et numberOfPlayers à (Request GameAnnounce ...)
          & gameSettings.gameCode        .~ gc
          & gameSettings.numberOfPlayers .~ fromMaybe 0 mn
        when (isNothing mn) $ logText %= (<>["oops! Le nombre de joueurs '" <> n <> "' n'a pas pu être résolu à un entier!"])
      _ -> return ()
    requestJoinGame = case args of
      [gc, playerName] -> do
        liftIO $ atomically $ modifyTVar gsTV $ \ gs -> gs
          & networkStatus                .~ Request JoinGame
          & myName                       .~ playerName
          -- TODO: ne pas utiliser gameSettings, mais passer directement gamecode à (Request GameAnnounce ...)
          & gameSettings.gameCode        .~ gc
      _ -> return ()
    resetNetwork = liftIO $ atomically $ modifyTVar gsTV $ networkStatus .~ Request ResetNetwork
    printGameState = do
      gs <- liftIO $ readTVarIO gsTV
      logText %= (<> lines (show gs))
    exec
      | cmd == "aide"                           = focusRing %= F.focusSetCurrent HelpBox
      | cmd `elem` [ "ag",  "announceGame"    ] = announceGame
      | cmd `elem` [ "rj",  "requestJoinGame" ] = requestJoinGame
      | cmd `elem` [ "rs",  "resetNetwork"    ] = resetNetwork
      | cmd `elem` [ "pgs", "printGameState"  ] = printGameState
      | cmd `elem` [ "cl",  "clearLog"        ] = clearLog
      | cmd `elem` [ "q",   "quit"            ] = M.halt
      | otherwise     = logText %= (<>["err.. impossible d'exécuter cette commande!"])
  unless (null cmdline) $ do
    logText %= (<>[">>> " <> cmdline])
    exec
    M.vScrollToEnd logViewportScroll
    clearEditorInputText

appEvent :: BrickEvent AppFocus () -> EventM AppFocus NodeState ()
appEvent ev = do
  let
    inputEvents (T.VtyEvent (V.EvKey V.KEnter []))             = executeCmd
    inputEvents (T.VtyEvent (V.EvKey V.KPageUp []))            = M.vScrollPage logViewportScroll T.Up
    inputEvents (T.VtyEvent (V.EvKey V.KPageDown []))          = M.vScrollPage logViewportScroll T.Down
    inputEvents (T.VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl])) = clearLog
    inputEvents (T.VtyEvent (V.EvKey (V.KChar '?') [] ))       = focusRing %= F.focusSetCurrent HelpBox
    inputEvents ev'                                            = T.zoom inputEditor $ E.handleEditorEvent ev'

    helpBoxEvents :: BrickEvent AppFocus () -> EventM AppFocus NodeState ()
    helpBoxEvents (T.VtyEvent (V.EvKey V.KEsc []))         = focusRing %= F.focusSetCurrent Input
    helpBoxEvents (T.VtyEvent (V.EvKey (V.KChar 'q') [] )) = focusRing %= F.focusSetCurrent Input
    helpBoxEvents _                                        = return ()
  r <- use focusRing
  case F.focusGetCurrent r of
    Just Input   -> inputEvents ev
    Just HelpBox -> helpBoxEvents ev
    _            -> return ()

attrsMap :: AttrMap
attrsMap = attrMap defAttr [ ]

drawUI :: NodeState -> [Widget AppFocus]
drawUI ns = helpBox <> [mainUI]
  where
    mainUI           = vBox [ B.border $ viewport Log T.Vertical $ vBox $ map str $ ns ^. logText
                        , B.border $ str "node> " <+> vLimit 1 inputTextBox
                        ]
    inputTextBox     = F.withFocusRing (ns^.focusRing) (E.renderEditor (str . head)) (ns^.inputEditor)
    cmdsHelpText     = B.borderWithLabel (str "COMMANDES") $ str $ unlines
                       [ "aide"
                       , "  Affiche l'aide de cet utilitaire."
                       , ""
                       , "ag {n} {code} {nomHôte}"
                       , "announceGame {n} {code} {nomHôte}"
                       , "  Annonce une partie à {n} joueurs sur le réseau"
                       , "  avec un code {code} et un nom de joueur de"
                       , "  l'hôte {nomHôte}."
                       , ""
                       , "rj {code} {nomJoueur}"
                       , "requestJoinGame {code} {nomJoueur}"
                       , "  Demande d'accéder à la partie en tant que {nomJoueur}"
                       , "  en utilisant le code {code}."
                       , ""
                       , "rs"
                       , "resetNetwork"
                       , "  Réinitialisation du réseau et de l'état du jeu."
                       , ""
                       , "pgs"
                       , "printGameState"
                       , "  Affiche l'état actuel du jeu."
                       , ""
                       , "cl"
                       , "clearLog"
                       , "  Efface le contenu du journal."
                       , ""
                       , "q"
                       , "quit"
                       , "  Quitter le programme."
                       ]
    keybindsHelpText = B.borderWithLabel (str "TOUCHES") $ str $ unlines
                       [ "?:      Affiche l'aide de cet utilitaire."
                       , "CTRL-l: Efface le contenu du journal."
                       , "ESC, q: Quitter la fenêtre."
                       ]
    helpBox      = case F.focusGetCurrent (ns ^. focusRing) of
                     Just HelpBox -> [ C.centerLayer $ hLimit 60 $ vBox [ C.hCenter cmdsHelpText
                                                                        , C.hCenter keybindsHelpText
                                                                        ]
                                     ]
                     _            -> []


app :: M.App NodeState () AppFocus
app = M.App { M.appDraw         = drawUI
            , M.appChooseCursor = F.focusRingCursor (^.focusRing)
            , M.appHandleEvent  = appEvent
            , M.appStartEvent   = focusRing %= F.focusSetCurrent Input
            , M.appAttrMap      = const attrsMap
            }

myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  void $ forkFinally io (\_ -> putMVar mvar ())
  return mvar

main :: IO ()
main = do
  gsTV <- newTVarIO defaultOnlineGameState
  mv   <- myForkIO $ runReaderT (Network.loop def) gsTV
  void $ M.defaultMain app $ NodeState { _focusRing   = F.focusRing $ enumFrom minBound
                                       , _inputEditor = E.editor Input Nothing ""
                                       , _logText     = []
                                       , _gameStateTV = gsTV
                                       }
  atomically $ modifyTVar gsTV $ networkStatus .~ ShuttingDown
  readMVar mv

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

