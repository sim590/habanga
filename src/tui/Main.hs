
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Default

import Control.Monad
import Control.Lens

import Brick.AttrMap
import Brick.Types ( BrickEvent
                   , EventM
                   )
import qualified Brick.Main as M

import Graphics.Vty (defAttr)

import ProgramState
import Widgets

import qualified MainMenu

import Paths_habanga

appEvent :: BrickEvent () e -> EventM () ProgramState ()
appEvent e = use currentScreen >>= \ case
  Just MainMenu -> MainMenu.event e
  _             -> return ()

attrsMap :: AttrMap
attrsMap = attrMap defAttr $  buttonAttrs
                           <> MainMenu.attrs

app :: M.App ProgramState () ()
app = M.App { M.appDraw         = \ ps -> map ($ ps) [MainMenu.widget]
            , M.appChooseCursor = M.neverShowCursor
            , M.appHandleEvent  = appEvent
            , M.appStartEvent   = return ()
            , M.appAttrMap      = const attrsMap
            }

main :: IO ()
main = do
  theMenuGameTitle   <- readFile =<< getDataFileName _HABANGA_MENU_GAMETITLE_FILE_PATH_
  theBlueCard35x53   <- readFile =<< getDataFileName _HABANGA_BLUECARD_35X53_
  thePurpleCard35x53 <- readFile =<< getDataFileName _HABANGA_PURPLECARD_35X53_
  void $ M.defaultMain app def { _gameResources = def { _menuGameTitle   = theMenuGameTitle
                                                      , _blueCard35x53   = theBlueCard35x53
                                                      , _purpleCard35x53 = thePurpleCard35x53
                                                      }
                               , _currentScreen = Just MainMenu
                               }

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

