
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

import Control.Lens
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.IO.Class

import Brick.Util (on)
import Brick.BChan
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
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Config as VConfig

import ProgramState
import NetworkState
import Widgets

import qualified MainMenu
import qualified GameView
import qualified BrickNetworkBridge as BNB

appEvent :: TVar NetworkState -> BrickEvent AppFocus BNB.NetworkBrickEvent -> EventM AppFocus ProgramState ()
appEvent nsTV e = use currentFocus >>= \ cf -> case focusGetCurrent cf of
  Just (MainMenu _) -> MainMenu.event nsTV e
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

app :: TVar NetworkState -> M.App ProgramState BNB.NetworkBrickEvent AppFocus
app nsTV = M.App { M.appDraw         = drawUI
                 , M.appChooseCursor = appChooseCursor
                 , M.appHandleEvent  = appEvent nsTV
                 , M.appStartEvent   = currentFocus %= focusSetCurrent (MainMenu MainMenuButtons)
                 , M.appAttrMap      = const attrsMap
                 }

main :: IO ()
main = do
  nsTV             <- liftIO (newTVarIO def)
  brickchan <- liftIO $ newBChan 10

  let buildVty = mkVty VConfig.defaultConfig
  initialVty        <- buildVty
  finalProgramState <- M.customMain initialVty buildVty (Just brickchan) (app nsTV) $ def { _brickEventChannel = Just brickchan }

  atomically $ modifyTVar nsTV $ status .~ ShuttingDown
  maybe (return ()) readMVar $ finalProgramState ^. networkMV
  maybe (return ()) readMVar $ finalProgramState ^. brickNetworkBridgeMV

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

