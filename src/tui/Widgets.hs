
{-|
  Module      : Widgets
  Description : Bouts de code réutilisables de l'interface texte
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module Widgets ( NamedButton
               , button
               , buttonAttrs
               , popUpWidgetDimensions
               ) where

import Brick.AttrMap
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core ( hLimit
                          , vLimit
                          , withAttr
                          , str
                          )

import qualified Graphics.Vty as V

-- Signature de `button` avec son premier paramètre appliqué.
type NamedButton n = Int -> Int -> Int -> AttrName -> Widget n

popUpWidgetDimensions :: Widget n -> Widget n
popUpWidgetDimensions = hLimit 40 . vLimit 5

buttonAttrs :: [(AttrName, V.Attr)]
buttonAttrs = [ ]

button
  :: String -- ^ Le texte du bouton.
  -> Int    -- ^ L'index dans le menu.
  -> Int    -- ^ La largeur du bouton en nombre de caractères.
  -> Int    -- ^ L'index du curseur dans le menu
  -> AttrName  -- ^ Couleur du texte du bouton lorsque sélectionné.
  -> Widget n
button s i w mi attrn = style $ str s
  where style      = selectAttr . B.border . hLimit w . C.hCenter
        selectAttr = if mi == i then withAttr attrn
                                else id

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

