
module Widgets where

import Control.Lens

import Brick.Util (on)
import Brick.AttrMap
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core ( hLimit
                          , withAttr
                          , str
                          , visible
                          )

import qualified Graphics.Vty as V

import ProgramState

buttonAttrs :: [(AttrName, V.Attr)]
buttonAttrs = [ (attrName "selectedAttr", V.black `on` V.white) ]

button
  :: String       -- ^ Le texte du bouton.
  -> Int          -- ^ L'index dans le menu.
  -> Int          -- ^ La largeur du bouton en nombre de caractères.
  -> ProgramState -- ^ L'état du programme.
  -> Widget n
button s i w ps = style $ str s
  where style      = selectAttr . B.border . hLimit w . C.hCenter
        selectAttr = if ps^.mainMenuState.menuIndex == i then withAttr (attrName "selectedAttr") . visible
                                                         else id

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

