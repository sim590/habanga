
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
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

import Control.Monad
import Control.Lens

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

import ProgramState
import Widgets

import qualified MainMenu
import qualified GameView

import Paths_habanga

appEvent :: BrickEvent AppFocus () -> EventM AppFocus ProgramState ()
appEvent e = use currentFocus >>= \ cf -> case focusGetCurrent cf of
  Just (MainMenu _) -> MainMenu.event e
  Just (Game     _) -> GameView.event e
  _                 -> return ()

appChooseCursor :: ProgramState -> [CursorLocation AppFocus] -> Maybe (CursorLocation AppFocus)
appChooseCursor ps = case focusGetCurrent (ps ^. currentFocus) of
  Just (MainMenu _) -> focusRingCursor formFocus $ (ps ^. mainMenuState . submenu) ^?! _Just . gameForm
  _                 -> const Nothing

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

loadProgramResources :: IO (Map.Map String String)
loadProgramResources = do
  let f acc p@(_,v) = (:acc) . (\ s -> set _2 s p) <$> loadResourceContent v
      loadResourceContent = readFile <=< getDataFileName
  Map.fromList <$> foldM f [] [ ("theMenuGameTitle",    _HABANGA_MENU_GAMETITLE_FILE_PATH_)
                              , ("theBlueCard35x53",    _HABANGA_BLUECARD_35X53_          )
                              , ("thePurpleCard35x53",  _HABANGA_PURPLECARD_35X53_        )
                              , ("theBlueCard10x15",    _HABANGA_BLUECARD_10X15           )
                              , ("theRedCard10x15",     _HABANGA_REDCARD_10X15            )
                              , ("theYellowCard10x15",  _HABANGA_YELLOWCARD_10X15         )
                              , ("thePurpleCard10x15",  _HABANGA_PURPLECARD_10X15         )
                              ]

main :: IO ()
main = do
  resMap <- loadProgramResources
  void $ M.defaultMain app def { _programResources = def { _menuGameTitle   = resMap!"theMenuGameTitle"
                                                         , _blueCard35x53   = resMap!"theBlueCard35x53"
                                                         , _purpleCard35x53 = resMap!"thePurpleCard35x53"
                                                         , _blueCard10x15   = resMap!"theBlueCard10x15"
                                                         , _redCard10x15    = resMap!"theRedCard10x15"
                                                         , _yellowCard10x15 = resMap!"theYellowCard10x15"
                                                         , _purpleCard10x15 = resMap!"thePurpleCard10x15"
                                                         }
                               }

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

