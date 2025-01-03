
module MainMenu ( event
                , attrs
                , widget
                ) where

import Control.Lens

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
attrs = [ ]

goUp :: T.EventM () ProgramState ()
goUp   = mainMenuState.mainMenuIndex %= max 0 . subtract 1

goDown :: T.EventM () ProgramState ()
goDown = mainMenuState.mainMenuIndex %= min (length buttons - 1) . (+ 1)

event :: T.BrickEvent () e -> T.EventM () ProgramState ()
event (T.VtyEvent (V.EvKey V.KDown       [] )) = goDown
event (T.VtyEvent (V.EvKey (V.KChar 'j') [] )) = goDown
event (T.VtyEvent (V.EvKey V.KUp         [] )) = goUp
event (T.VtyEvent (V.EvKey (V.KChar 'k') [] )) = goUp
event (T.VtyEvent (V.EvKey V.KEsc        [] )) = M.halt
event (T.VtyEvent (V.EvKey (V.KChar 'q') [] )) = M.halt
event _                                        = return ()

buttons :: [Int -> Int -> Int -> AttrName -> Widget n]
buttons = [ button "Jouer"
          , button "Options"
          , button "Quitter"
          ]

widget :: ProgramState -> Widget ()
widget ps = hBox [ leftPanel
                 , hLimit titleWidth middlePanel
                 , rightPanel
                 ]
  where sidePanelStyle a = C.center . withBorderStyle BS.unicodeRounded . B.border . withAttr (attrName a) . str
        leftPanel        = sidePanelStyle "bluecard"   $ ps^.programResources.blueCard35x53
        rightPanel       = sidePanelStyle "purplecard" $ ps^.programResources.purpleCard35x53

        middlePanel      = vBox [ C.hCenter $ str $ ps^.programResources.menuGameTitle
                                , C.hCenter $ B.border $ hLimit titleWidth $ C.center menuPanel
                                ]
        menuPanel        = vBox [ C.hCenter $ str "Menu principal"
                                , C.center menuOptions
                                ]
        menuOptions      = vBox $ map C.center $ appendArgsToButtons buttons 25

        titleWidth       = length $ head $ lines (ps^.programResources.menuGameTitle)

        -- Cette fonction est un peu moins évidente, mais elle assure que les
        -- indices des boutons sont bien attribués et que la largeur de ceux-ci
        -- est la même pour tous.
        appendArgsToButtons bs width = fst (foldl (\ (l, i) f -> (l++[f i], i+1)) ([], 0) bs) <*> [width]
                                                                                              <*> [ps^.mainMenuState.mainMenuIndex] <*> [attrName "selectedAttr"]

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

