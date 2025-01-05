
module GameView ( event
                , attrs
                , widget
                ) where

import Control.Lens

import Brick.AttrMap
import qualified Brick.Types as T
import Brick.Types (Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core ( hLimit
                          , vLimit
                          , withAttr
                          , str
                          , vBox
                          , hBox
                          )

import qualified Graphics.Vty as V

import ProgramState
import Widgets

import Cards
import GameState

colorAttrFromCard :: Card -> Bool -> AttrName
colorAttrFromCard c selected
  | selected = case c^.color of
      Just Red    -> attrName "redcardSelected"
      Just Yellow -> attrName "yellowcardSelected"
      Just Blue   -> attrName "bluecardSelected"
      Just Purple -> attrName "purplecardSelected"
      Nothing     -> attrName "greycardSelected"
  | otherwise = case c^.color of
      Just Red    -> attrName "redcard"
      Just Yellow -> attrName "yellowcard"
      Just Blue   -> attrName "bluecard"
      Just Purple -> attrName "purplecard"
      Nothing     -> attrName "greycard"

attrs :: [(AttrName, V.Attr)]
attrs = [ ]

event :: T.BrickEvent AppFocus () -> T.EventM AppFocus ProgramState ()
event (T.VtyEvent (V.EvKey V.KRight      [] )) = goRight
event (T.VtyEvent (V.EvKey (V.KChar 'l') [] )) = goRight
event (T.VtyEvent (V.EvKey V.KLeft       [] )) = goLeft
event (T.VtyEvent (V.EvKey (V.KChar 'h') [] )) = goLeft
event (T.VtyEvent (V.EvKey (V.KChar 'q') [] )) = currentScreen .= Just MainMenu
event _                                        = return ()

goLeft :: T.EventM AppFocus ProgramState ()
goLeft   = gameViewState.gameViewIndex %= max 0 . subtract 1

goRight :: T.EventM AppFocus ProgramState ()
goRight = do
  gs <- use gameState
  gameViewState.gameViewIndex %= min ((+ (-1)) $ length $ head (gs^.players) ^. cardsInHand) . (+ 1)

widget :: ProgramState -> [Widget AppFocus]
widget ps = [ vBox [ C.hCenter $ hBox $ playerCardsButtons currentCardsInHand
                   , C.center  $ vBox $ map hBox cardsOnTableMatrix
                   ]
            ]
  where
    btn i c            = button (show $ c^.value) i 15 (ps^.gameViewState.gameViewIndex) (colorAttrFromCard c True)
    playerCardsButtons = zipWith (\ i c -> C.hCenter $ withAttr (colorAttrFromCard c False) $ btn i c) [0..]
    currentPlayer      = head $ ps^.gameState.players
    currentCardsInHand = currentPlayer^.cardsInHand
    theCardsOnTable    = ps^.gameState.cardsOnTable
    cardWidget c       = C.vCenter $ B.border $ vLimit 1 $ hLimit 2 $ C.center $ withAttr (colorAttrFromCard c False) $ str $ show $ c^.value
    centralCardWidget  = B.border . hLimit 15 . C.center . str
    cardsOnTableMatrix = [ [ cardWidget $ theCardsOnTable^.red._1
                           , withAttr (attrName "bluecard") $ centralCardWidget $ ps^.programResources.blueCard10x15
                           , cardWidget $ theCardsOnTable^.red._2
                           ]
                         , [ cardWidget $ theCardsOnTable^.yellow._1
                           , withAttr (attrName "redcard") $ centralCardWidget $ ps^.programResources.redCard10x15
                           , cardWidget $ theCardsOnTable^.yellow._2
                           ]
                         , [ cardWidget $ theCardsOnTable^.blue._1
                           , withAttr (attrName "yellowcard") $ centralCardWidget $ ps^.programResources.yellowCard10x15
                           , cardWidget $ theCardsOnTable^.blue._2
                           ]
                         , [ cardWidget $ theCardsOnTable^.purple._1
                           , withAttr (attrName "purplecard") $ centralCardWidget $ ps^.programResources.purpleCard10x15
                           , cardWidget $ theCardsOnTable^.purple._2
                           ]
                         ]

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

