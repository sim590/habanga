
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Default
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map

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

loadGameResources :: IO (Map.Map String String)
loadGameResources = do
  let f acc p@(_,v) = (:acc) . (\ s -> set _2 s p) <$> loadResourceContent v
      loadResourceContent = readFile <=< getDataFileName
  Map.fromList <$> foldM f [] [ ("theMenuGameTitle",   _HABANGA_MENU_GAMETITLE_FILE_PATH_)
                              , ("theBlueCard35x53",   _HABANGA_BLUECARD_35X53_          )
                              , ("thePurpleCard35x53", _HABANGA_PURPLECARD_35X53_        )
                              ]

main :: IO ()
main = do
  resMap <- loadGameResources
  void $ M.defaultMain app def { _gameResources = def { _menuGameTitle   = resMap!"theMenuGameTitle"
                                                      , _blueCard35x53   = resMap!"theBlueCard35x53"
                                                      , _purpleCard35x53 = resMap!"thePurpleCard35x53"
                                                      }
                               , _currentScreen = Just MainMenu
                               }

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

