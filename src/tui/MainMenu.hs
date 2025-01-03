
module MainMenu ( event
                , attrs
                , widget
                ) where

import Control.Lens

import Brick.Util (on)
import Brick.AttrMap
import qualified Brick.Types as T
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
import Widgets


attrs :: [(AttrName, V.Attr)]
attrs = [ (attrName "blueCard35x53",   V.blue    `on` V.black)
        , (attrName "purpleCard35x53", V.magenta `on` V.black)
        ]

goUp :: T.EventM () ProgramState ()
goUp   = mainMenuState.menuIndex %= max 0 . subtract 1

goDown :: T.EventM () ProgramState ()
goDown = mainMenuState.menuIndex %= min 2 . (+ 1)

event :: T.BrickEvent () e -> T.EventM () ProgramState ()
event (T.VtyEvent (V.EvKey V.KDown       [] )) = goDown
event (T.VtyEvent (V.EvKey (V.KChar 'j') [] )) = goDown
event (T.VtyEvent (V.EvKey V.KUp         [] )) = goUp
event (T.VtyEvent (V.EvKey (V.KChar 'k') [] )) = goUp
event (T.VtyEvent (V.EvKey V.KEsc        [] )) = M.halt
event (T.VtyEvent (V.EvKey (V.KChar 'q') [] )) = M.halt
event _                                        = return ()

widget :: ProgramState -> Widget ()
widget ps = ui
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
        buttons          = [ button "Jouer"
                           , button "Options"
                           , button "Quitter"
                           ]
        menuOptions      = vBox $ map C.center $ appendArgsToButtons buttons 25

        titleWidth       = length $ head $ lines (ps^.gameResources.menuGameTitle)

        -- Cette fonction est un peu moins évidente, mais elle assure que les
        -- indices des boutons sont bien attribués et que la largeur de ceux-ci
        -- est la même pour tous.
        appendArgsToButtons bs width = fst (foldl (\ (l, i) f -> (l++[f i], i+1)) ([], 0) bs) <*> [width] <*> [ps]

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

