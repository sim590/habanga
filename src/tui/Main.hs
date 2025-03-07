
{-|
  Module      : Main
  Description : Point d'entrée de l'interface texte du jeu
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module Main where

import Data.Maybe
import Data.Default

import Control.Monad
import Control.Monad.Reader
import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM

import Brick.Util (on)
import Brick.Focus
import Brick.Forms
import Brick.AttrMap
import Brick.Types ( BrickEvent
                   , EventM
                   , Widget
                   , CursorLocation
                   )
import qualified Brick.Main as M

import Graphics.Vty (defAttr)
import qualified Graphics.Vty as V

import OpenDHT.DhtRunner ( dhtConfig
                         , nodeConfig
                         , persistPath
                         )

import System.Directory

import GameState
import ProgramState
import Network
import NetworkState
import Widgets

import qualified MainMenu
import qualified GameView

myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  void $ forkFinally io (\_ -> putMVar mvar ())
  return mvar

appEvent :: BrickEvent AppFocus () -> EventM AppFocus ProgramState ()
appEvent e = use currentFocus >>= \ cf -> case focusGetCurrent cf of
  Just (MainMenu _) -> MainMenu.event e
  Just (Game     _) -> GameView.event e
  _                 -> return ()

appChooseCursor :: ProgramState -> [CursorLocation AppFocus] -> Maybe (CursorLocation AppFocus)
appChooseCursor ps = case focusGetCurrent (ps ^. currentFocus) of
  Just (MainMenu (GameInitializationForm _))       -> focusRingCursor formFocus $ (ps ^. mainMenuState . submenu) ^?! _Just . gameForm
  Just (MainMenu (OnlineGameInitializationForm _)) -> focusRingCursor formFocus $ (ps ^. mainMenuState . submenu) ^?! _Just . onlineGameForm
  _                                                -> const Nothing

attrsMap :: AttrMap
attrsMap = attrMap defAttr $ [ (attrName "selectedAttr",       V.black   `on` V.white)
                             , (attrName "bluecardSelected",   V.blue    `on` V.white)
                             , (attrName "bluecard",           V.blue    `on` V.black)
                             , (attrName "greycardSelected",   V.black   `on` V.white)
                             , (attrName "greycard",           V.white   `on` V.black)
                             , (attrName "purplecardSelected", V.magenta `on` V.white)
                             , (attrName "purplecard",         V.magenta `on` V.black)
                             , (attrName "redcardSelected",    V.red     `on` V.white)
                             , (attrName "redcard",            V.red     `on` V.black)
                             , (attrName "yellowcardSelected", V.yellow  `on` V.white)
                             , (attrName "yellowcard",         V.yellow  `on` V.black)
                             ]
                           <> buttonAttrs
                           <> MainMenu.attrs
                           <> GameView.attrs

drawUI :: ProgramState -> [Widget AppFocus]
drawUI ps = case focusGetCurrent (ps^.currentFocus) of
  Just (MainMenu _) -> MainMenu.widget ps
  Just (Game     _) -> GameView.widget ps
  s                 -> error $ "drawUI: l'écran '" <> show (fromJust s) <> "' n'est pas implanté!"

app :: M.App ProgramState () AppFocus
app = M.App { M.appDraw         = drawUI
            , M.appChooseCursor = appChooseCursor
            , M.appHandleEvent  = appEvent
            , M.appStartEvent   = currentFocus %= focusSetCurrent (MainMenu MainMenuButtons)
            , M.appAttrMap      = const attrsMap
            }

main :: IO ()
main = do
  gs               <- defaultOnlineGameState
  habangaCachePath <- getXdgDirectory XdgCache "habanga"
  createDirectoryIfMissing True habangaCachePath

  let dhtRunnerConf = def & dhtConfig.nodeConfig.persistPath .~ (habangaCachePath <> "/dht.cache")
  -- mv <- myForkIO $ runReaderT (Network.loop dhtRunnerConf) (gs^?!networkState)

  void $ M.defaultMain app def { _programResources = def }

  -- atomically $ modifyTVar (gs^?!networkState) $ status .~ ShuttingDown
  -- readMVar mv

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

