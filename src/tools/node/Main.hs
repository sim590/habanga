
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

import qualified Data.List as List
import Data.Default
import Data.Maybe
import qualified Data.Map as Map

import Text.Read

import Numeric

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM

import System.Random

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

import Random
import Cards
import Game
import GameState
import Network

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
    logInvalidParameter = logText %= (<>["err.. Un des paramètres est invalide!"])
    announceGame = case args of
      [n, gc, playerName] -> do
        let mn              = readMaybe n
            theGameSettings = OnlineGameSettings gc (fromMaybe 0 mn)
        liftIO $ atomically $ modifyTVar gsTV $ networkStatus .~ Request (GameAnnounce theGameSettings playerName)
        when (isNothing mn) $ logText %= (<>["err.. Le nombre de joueurs '" <> n <> "' n'a pas pu être résolu à un entier!"])
      _ -> logInvalidParameter
    requestJoinGame = case args of
      [gc, playerName] -> liftIO $ atomically $ modifyTVar gsTV $ networkStatus .~ Request (JoinGame gc playerName)
      _                -> logInvalidParameter
    startGame = liftIO $ do
      gs <- readTVarIO gsTV
      let
        sortedPlayerNames   = List.sort $ Map.elems $ gs ^. playersIdentities
        gen                 = mkStdGen (fst $ head $ readHex $ gs^.gameSettings.gameCode)
        shuffledPlayerNames = deterministiclyShuffle sortedPlayerNames gen
      gs' <- reInitialize shuffledPlayerNames gs gen
      atomically $ modifyTVar gsTV $ const $ gs' & networkStatus .~ Request GameStart
    playTurn = case args of
      [n, strColor, slot] -> do
        let
          modifyGs gs' = case mPlayerTurn of
           Just pt -> gs' & networkStatus .~ Request pt
           _       -> gs'
          mPlayerTurn = do
            c    <- readMaybe strColor
            n'   <- readMaybe n
            let
              mCard
                | slot `elem` [ "l", "left"  ] = Just $ Left  (Card n' (Just c))
                | slot `elem` [ "r", "right" ] = Just $ Right (Card n' (Just c))
                | otherwise                    = Nothing
            PlayTurn <$> mCard
        liftIO (readTVarIO gsTV) >>= \ gs -> case mPlayerTurn of
          Just (PlayTurn ec) -> do
            let (c, b) = fromEitherCard ec
            (mNumberOfCardsDrawn, gs') <- flip runStateT gs $ runMaybeT $ processPlayerTurnAction c b
            case mNumberOfCardsDrawn of
              Just _  -> liftIO $ atomically $ modifyTVar gsTV $ const $ modifyGs gs'
              Nothing -> logText %= (<>["err.. Impossible de jouer cette carte!"])
          _ -> logInvalidParameter
      _ -> logInvalidParameter
    processOtherPlayerTurn = liftIO (readTVarIO gsTV) >>= \ gs -> do
      let
        consecutivePlayerTurns' = consecutivePlayerTurns gs (gs ^. gameTurns)
      case consecutivePlayerTurns' of
        ((n, turn):_) -> do
          gs' <- flip execStateT gs $ do
            let (c, b) = fromEitherCard turn
            runMaybeT $ processPlayerTurnAction c b
          liftIO $ atomically $ modifyTVar gsTV $ const $ gs' & gameTurns %~ Map.delete n
        _ -> logText %= (<>["err.. Aucun tour à traiter!"])
    resetNetwork = liftIO $ atomically $ modifyTVar gsTV $ networkStatus .~ Request ResetNetwork
    printGameState = do
      gs <- liftIO $ readTVarIO gsTV
      logText %= (<> lines (show gs))
    exec
      | cmd == "aide"                                  = focusRing %= F.focusSetCurrent HelpBox
      | cmd `elem` [ "ag",  "announceGame"           ] = announceGame
      | cmd `elem` [ "rj",  "requestJoinGame"        ] = requestJoinGame
      | cmd `elem` [ "sg",  "startGame"              ] = startGame
      | cmd `elem` [ "pt",  "playTurn"               ] = playTurn
      | cmd `elem` [ "po",  "processOtherPlayerTurn" ] = processOtherPlayerTurn
      | cmd `elem` [ "rs",  "resetNetwork"           ] = resetNetwork
      | cmd `elem` [ "pgs", "printGameState"         ] = printGameState
      | cmd `elem` [ "cl",  "clearLog"               ] = clearLog
      | cmd `elem` [ "q",   "quit"                   ] = M.halt
      | otherwise                                      = logText %= (<>["err.. impossible d'exécuter cette commande!"])
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
                       , "sg"
                       , "startGame"
                       , "  Initialise l'état du jeu et prépare la couche réseau."
                       , ""
                       , "pt"
                       , "playTurn {numéro} {couleur} {emplacement}"
                       , "  Jouer à son tour une carte de numéro {numéro} et de couleur"
                       , "  {couleur} à l'emplacement {emplacement} (gauche/droite)."
                       , ""
                       , "po"
                       , "processOtherPlayerTurn"
                       , "  Joue le tour d'un autre joueur. Les tours sont accumulés"
                       , "  en cache. Cette commande doit être exécutée pour chaque tour"
                       , "  des autres joueurs. Celle-ci n'aura un effet que si le tour"
                       , "  du joueur a déjà été reçu."
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

