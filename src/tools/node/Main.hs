
{-|
  Module      : Main
  Description : Point d'entrée de l'outil habanga-node
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Default
import Data.Maybe

import Text.Read

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
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

import Cards
import Game
import GameState
import Network
import NetworkState
import qualified OnlineGame

import System.Environment (lookupEnv)
import I18n
import I18n.Types

data AppFocus = Input
              | Log
              | HelpBox
  deriving (Ord, Show, Eq, Enum, Bounded)
data NodeState = NodeState { _focusRing                 :: F.FocusRing AppFocus
                           , _inputEditor               :: E.Editor String AppFocus
                           , _logText                   :: [String]
                           , _gameState                 :: GameState
                           , _networkState              :: NetworkState
                           , _nodeNetworkUpdateChannel  :: TChan NetworkChannelUpdate
                           , _nodeNetworkRequestChannel :: TChan NetworkRequest
                           , _translations              :: Translations
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
  ie      <- use inputEditor
  gs      <- use gameState
  netChan <- use nodeNetworkUpdateChannel
  reqChan <- use nodeNetworkRequestChannel
  liftIO (atomically $ tryReadTChan netChan) >>= \ case
    Just (NetworkChannelUpdate netState) -> networkState .= netState
    Nothing                              -> return ()
  netState <- use networkState
  t       <- use translations
  let
    cmdline      = head $ E.getEditContents ie
    cmdlineToks  = words cmdline
    cmd          = head cmdlineToks
    args         = tail cmdlineToks
    logInvalidParameter = logText %= (<>[_errInvalidParameter t])
    announceGame = case args of
      [n, gc, playerName] -> do
        let mn              = readMaybe n
            theGameSettings = OnlineGameSettings gc (fromMaybe 0 mn)
        liftIO $ atomically $ writeTChan reqChan $ GameAnnounce theGameSettings playerName
        when (isNothing mn) $ logText %= (<>[_errInvalidPlayerCount t n])
      _ -> logInvalidParameter
    requestJoinGame = case args of
      [gc, playerName] -> liftIO $ atomically $ writeTChan reqChan $ JoinGame gc playerName
      _                -> logInvalidParameter
    startGame = do
      let
        shuffPNames = OnlineGame.shuffledPlayerNames netState gen
        gen         = OnlineGame.randomGeneratorFromNetworkState netState
      gs' <- liftIO $ initialize shuffPNames gen
      gameState .= gs'
      let
        myRank = fromJust $ playerRank (netState^.myName) gs'
      liftIO $ atomically $ writeTChan reqChan $ GameStart myRank
    playTurn = case args of
      [n, strColor, slot] -> do
        let
          mPlayerTurn = do
            c    <- readMaybe strColor
            n'   <- readMaybe n
            let
              mCard
                | slot `elem` [ "l", "left"  ] = Just $ Left  (Card n' (Just c))
                | slot `elem` [ "r", "right" ] = Just $ Right (Card n' (Just c))
                | otherwise                    = Nothing
            PlayTurn <$> mCard
        case mPlayerTurn of
          Just pt@(PlayTurn ec) -> do
            (mNumberOfCardsDrawn, gs') <- flip runStateT gs $ runMaybeT $ processPlayerTurnAction ec
            case mNumberOfCardsDrawn of
              Just nCardsToDraw  -> do
                when (nCardsToDraw > 0) $
                  logText %= (<> [_msgDrawCards t nCardsToDraw])
                gameState .= gs'
                liftIO $ atomically $ writeTChan reqChan pt
              Nothing -> logText %= (<>[_errCannotPlayCard t])
          _ -> logInvalidParameter
      _ -> logInvalidParameter
    processOtherPlayerTurn = do
      let
        consecutivePlayerTurns' = consecutivePlayerTurns netState (netState ^. gameTurns)
      case consecutivePlayerTurns' of
        ((_, ec):_) -> do
          gs' <- flip execStateT gs $ do
            runMaybeT $ processPlayerTurnAction ec
          gameState .= gs'
          liftIO $ atomically $ writeTChan reqChan $ UpdateTurnNumber $ netState ^. turnNumber + 1
        _ -> logText %= (<>[_errNoTurnToProcess t])
    resetNetwork = liftIO $ atomically $ writeTChan reqChan ResetNetwork
    printGameState = do
      logText %= (<> lines (show gs))
      logText %= (<> lines (show netState))
    exec
      | cmd == _helpUsageHelp t                        = focusRing %= F.focusSetCurrent HelpBox
      | cmd `elem` [ "ag",  "announceGame"           ] = announceGame
      | cmd `elem` [ "rj",  "requestJoinGame"        ] = requestJoinGame
      | cmd `elem` [ "sg",  "startGame"              ] = startGame
      | cmd `elem` [ "pt",  "playTurn"               ] = playTurn
      | cmd `elem` [ "po",  "processOtherPlayerTurn" ] = processOtherPlayerTurn
      | cmd `elem` [ "rs",  "resetNetwork"           ] = resetNetwork
      | cmd `elem` [ "pgs", "printGameState"         ] = printGameState
      | cmd `elem` [ "cl",  "clearLog"               ] = clearLog
      | cmd `elem` [ "q",   "quit"                   ] = M.halt
      | otherwise                                      = logText %= (<>[_errUnkownCommand t])
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
    t = ns ^. translations
    mainUI           = vBox [ B.border $ viewport Log T.Vertical $ vBox $ map str $ ns ^. logText
                            , B.border $ str "node> " <+> vLimit 1 inputTextBox
                            ]
    inputTextBox     = F.withFocusRing (ns^.focusRing) (E.renderEditor (str . head)) (ns^.inputEditor)
    cmdsHelpText     = B.borderWithLabel (str (_titleCommands t)) $ str $ unlines
                       [ _helpUsageHelp t
                       , _helpDescHelp t
                       , ""
                       , _helpUsageAnnounceGameShort t
                       , _helpUsageAnnounceGame t
                       , _helpDescAnnounceGame t
                       , ""
                       , _helpUsageJoinGameShort t
                       , _helpUsageJoinGame t
                       , _helpDescJoinGame t
                       , ""
                       , "sg"
                       , "startGame"
                       , _helpDescStartGame t
                       , ""
                       , "pt"
                       , _helpUsagePlayTurn t
                       , _helpDescPlayTurn t
                       , ""
                       , "po"
                       , "processOtherPlayerTurn"
                       , _helpDescProcessOtherTurn t
                       , ""
                       , "rs"
                       , "resetNetwork"
                       , _helpDescResetNetwork t
                       , ""
                       , "pgs"
                       , "printGameState"
                       , _helpDescPrintGameState t
                       , ""
                       , "cl"
                       , "clearLog"
                       , _helpDescClearLog t
                       , ""
                       , "q"
                       , "quit"
                       , _helpDescQuit t
                       ]
    keybindsHelpText = B.borderWithLabel (str (_titleKeyBindings t)) $ str $ unlines
                       [ _kbHelp t
                       , _kbClearLog t
                       , _kbCloseWindow t
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
  reqChan <- newTChanIO
  netChan <- newTChanIO
  mv      <- myForkIO $ Network.loop def (NetworkTwoWayChannel reqChan netChan)
  envMLang <- lookupEnv "LANG"
  let userLang = languageFromString envMLang
  void $ M.defaultMain app $ NodeState { _focusRing                 = F.focusRing $ enumFrom minBound
                                       , _inputEditor               = E.editor Input Nothing ""
                                       , _logText                   = []
                                       , _gameState                 = def
                                       , _networkState              = def
                                       , _nodeNetworkUpdateChannel  = netChan
                                       , _nodeNetworkRequestChannel = reqChan
                                       , _translations              = getTranslations userLang
                                       }
  atomically $ writeTChan reqChan Shutdown
  readMVar mv

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

