
module Main where

import Data.Default

import Control.Monad
import Control.Lens

import Brick.Util (on)
import Brick.AttrMap
import Brick.Types (Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core ( hLimit
                          , withAttr
                          , withBorderStyle
                          , str
                          , vBox
                          , hBox
                          )

import qualified Graphics.Vty as V

import ProgramState

import Paths_habanga

button :: String -> Int -> Widget n
button s w = B.border $ hLimit w $  C.hCenter $ str s

drawUI :: ProgramState -> [Widget ()]
drawUI ps = [ui]
  where ui = hBox [ leftPanel
                  , hLimit titleWidth middlePanel
                  , rightPanel
                  ]
        sidePanelStyle a = C.center . withBorderStyle BS.unicodeRounded . B.border . withAttr (attrName a) . str
        leftPanel        = sidePanelStyle "blueCard35x53"   $ ps^.gameResources.blueCard35x53
        rightPanel       = sidePanelStyle "purpleCard35x53" $ ps^.gameResources.purpleCard35x53

        middlePanel      = vBox [ C.hCenter $ str $ ps^.gameResources.menuGameTitle
                                , C.hCenter $ B.border $ hLimit titleWidth $ C.center menuPanel
                                ]
        menuPanel        = vBox [ C.hCenter $ str "Menu principal"
                                , C.center menuOptions
                                ]
        menuOptions      = vBox $ map C.center $ [ button "Jouer"
                                                 , button "Options"
                                                 , button "Quitter"
                                                 ] <*> [25]

        titleWidth       = length $ head $ lines (ps^.gameResources.menuGameTitle)

theMap :: AttrMap
theMap = attrMap V.defAttr [ (attrName "selectedAttr",    V.black   `on` V.white)
                           , (attrName "blueCard35x53",   V.blue    `on` V.black)
                           , (attrName "purpleCard35x53", V.magenta `on` V.black)
                           ]

app :: M.App ProgramState () ()
app = M.App { M.appDraw         = drawUI
            , M.appChooseCursor = M.neverShowCursor
            , M.appHandleEvent  = M.resizeOrQuit
            , M.appStartEvent   = return ()
            , M.appAttrMap      = const theMap
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
                               }

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

