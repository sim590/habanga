
{-|
  Module      : GameView
  Description : Interface texte de l'écran de jeu
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

{-# LANGUAGE LambdaCase #-}

module GameView ( event
                , attrs
                , widget
                ) where

import Data.Either.Extra
import qualified Data.Text as Text

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.Trans.Maybe

import Control.Concurrent.STM

import Brick.AttrMap
import Brick.Focus ( focusSetCurrent
                   )
import qualified Brick.Types as T
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core ( Padding (Pad)
                          , viewport
                          , translateBy
                          , padLeft
                          , hLimit
                          , vLimit
                          , withAttr
                          , str
                          , vBox
                          , hBox
                          , fill
                          , (<+>)
                          )

import qualified Graphics.Vty as V

import Widgets

import Cards
import GameState
import qualified Game
import NetworkState (NetworkState)
import qualified NetworkState as NS
import qualified OnlineGame
import ProgramState
import qualified BrickNetworkBridge as BNB

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

goLeft :: T.EventM AppFocus ProgramState ()
goLeft = gameViewState.gameViewIndex %= max 0 . subtract 1

-- TODO: ajuster pour récupérer la borne en fonction de nos cartes et pas celles du premier dans la liste.
goRight :: T.EventM AppFocus ProgramState ()
goRight = do
  gs <- use gameState
  gameViewState.gameViewIndex %= min ((+ (-1)) $ length $ head (gs^.players) ^. cardsInHand) . (+ 1)

goBackOrQuit :: T.EventM AppFocus ProgramState ()
goBackOrQuit = do
  gameViewState . winner  .= Nothing
  gameViewState . gameLog .= []
  currentFocus %= focusSetCurrent (MainMenu MainMenuButtons)

playCard :: Either Card Card -> T.EventM AppFocus ProgramState ()
playCard sidedCard = do
  thePlayers <- use (gameState . players)
  let
    currentPlayer = head thePlayers
    cardColorStr  = Text.unpack $ Text.toLower $ Text.pack $ maybe "gris" show (card^.color)
    card          = fromEither sidedCard

  cardsDrawn <- runMaybeT $ Game.processPlayerTurnAction sidedCard

  let
    cardsDrawnLog = [">> " <> "pige " <> show (cardsDrawn^?!_Just) <> " carte(s)!" | ((>0) <$> cardsDrawn) == Just True]
    playerLog     = unlines $ [ "Joueur " <> currentPlayer^.name <> ": "
                              , ">> "     <> "a joué le " <> show (card^.value) <> " " <> cardColorStr <> "."
                              ] <> cardsDrawnLog
                                <> ["\n"]
  gameViewState . gameLog %= (:) playerLog

  theWinner <- Game.winner
  gameViewState . winner .= ((^.name) <$> theWinner)

playMyTurn :: TVar NetworkState -> Either () () -> T.EventM AppFocus ProgramState ()
playMyTurn nsTV side = liftIO (readTVarIO nsTV) >>= \ ns -> when (OnlineGame.isMyTurn ns) $ do
  thePlayers <- use (gameState . players)
  let currentPlayer = head thePlayers
  cardIdx <- use (gameViewState . gameViewIndex)
  let
    card = (currentPlayer^.cardsInHand) !! cardIdx
    ec   = case side of
             Left {}  -> Left card
             Right {} -> Right card
  playCard ec
  case ns ^. NS.status of
    NS.Offline -> return ()
    _          -> liftIO $ atomically $ modifyTVar nsTV $ \ ns' -> ns' & NS.status     .~ NS.Request (NS.PlayTurn (ns' ^. NS.turnNumber) ec)
                                                                       & NS.turnNumber +~ 1


event :: TVar NetworkState -> T.BrickEvent AppFocus BNB.NetworkBrickEvent -> T.EventM AppFocus ProgramState ()
event nsTV (T.AppEvent (BNB.NetworkBrickUpdate ns)) = do
  liftIO $ appendFile ("/tmp/toto"<> ns ^. NS.myID <>".txt") $ show ns
  let (turns, ns') = OnlineGame.consumeConsecutivePlayerTurns ns
  forM_ turns $ \ (_, card) -> playCard card
  networkState .= ns'
  liftIO $ atomically $ modifyTVar nsTV $ NS.turnNumber .~ ns' ^. NS.turnNumber
event nsTV ev = do
  let
    quitOrNothing (T.VtyEvent (V.EvKey (V.KChar 'q') [] )) = goBackOrQuit
    quitOrNothing _                                        = return ()
    mainEvent (T.VtyEvent (V.EvKey (V.KChar 'z') [] )) = playMyTurn nsTV (Left ())
    mainEvent (T.VtyEvent (V.EvKey (V.KChar 'c') [] )) = playMyTurn nsTV (Right ())
    mainEvent (T.VtyEvent (V.EvKey V.KRight      [] )) = goRight
    mainEvent (T.VtyEvent (V.EvKey (V.KChar 'l') [] )) = goRight
    mainEvent (T.VtyEvent (V.EvKey V.KLeft       [] )) = goLeft
    mainEvent (T.VtyEvent (V.EvKey (V.KChar 'h') [] )) = goLeft
    mainEvent (T.VtyEvent (V.EvKey (V.KChar 'q') [] )) = goBackOrQuit
    mainEvent _                                        = return ()
  Game.winner >>= \ case
    Just _ -> quitOrNothing ev
    _      -> mainEvent ev

winnerDialog :: ProgramState -> [Widget AppFocus]
winnerDialog ps =
  let
    msg w = fill ' ' <+> str (w <> " a gagné!") <+> fill ' '
    dimensions = hLimit 40 . vLimit 5
   in case ps^.gameViewState.winner of
        Just winnerName -> [ C.centerLayer $ B.borderWithLabel (str "Fin de partie!")
                                           $ dimensions
                                           $ vBox [ fill ' ', msg winnerName, fill ' ' ]
                           ]
        _               -> []


widget :: ProgramState -> [Widget AppFocus]
widget ps = winnerDialog ps <> [gameLogWidget] <> gameUI
  where
    ns                 = ps ^. networkState
    players'           = ps ^. gameState . players
    gameUI             = [ vBox [ C.hCenter $ str $ "Joueur: " <> currentPlayer ^. name <> ", Tour: " <> show (ns ^. NS.turnNumber)
                                , C.hCenter $ hBox $ playerCardsButtons currentCardsInHand
                                , C.center (vBox $ map hBox cardsOnTableMatrix)
                                , C.hCenter $ B.borderWithLabel (str "Touches") $ vLimit (length keyBindText) keybindBox
                                ]
                         ]
    gameLogTextWidget  = vBox $ map str $ ps^.gameViewState.gameLog
    gameLogWidget      = translateBy (T.Location (1, 10)) $ B.borderWithLabel (str "Journal du jeu")
                                                          $ vLimit 15
                                                          $ hLimit 30
                                                          $ viewport (Game (Just GameLog)) T.Vertical gameLogTextWidget
    keybindBox         = vBox $ over traverse (hLimit 50 . hBox)
                              $ over (traverse.ix 0) (\ w ->  padLeft (Pad 2) w <+> fill ' ')
                              $ over (traverse.ix 1) (\ w -> w <+> fill ' ')
                              $ over (traverse.traverse) str keyBindText
    keyBindText        = [ ["gauche/droite", "Sélectionner une carte"]
                         , ["(z)",           "Jouer à gauche"        ]
                         , ["(c)",           "Jouer à droite"        ]
                         , ["(q)",           "Quitter"               ]
                         ]
    btn i c            = button (show $ c^.value) i 15 (ps^.gameViewState.gameViewIndex) (colorAttrFromCard c True)
    playerCardsButtons = zipWith (\ i c -> C.hCenter $ withAttr (colorAttrFromCard c False) $ btn i c) [0..]
    currentPlayer      = head players'
    thisPlayer         = case ns ^. NS.status of
                           NS.Offline -> currentPlayer
                           _          -> players' !! OnlineGame.myCurrentPosInPlayerList ns
    currentCardsInHand = thisPlayer ^. cardsInHand
    theCardsOnTable    = ps^.gameState.cardsOnTable
    cardWidget c       = C.vCenter $ B.border $ vLimit 1 $ hLimit 2 $ C.center $ withAttr (colorAttrFromCard c False) $ str $ show $ c^.value
    centralCardWidget  = B.border . hLimit 15 . C.center . str
    cardsOnTableMatrix = [ [ cardWidget $ theCardsOnTable^.red._1
                           , withAttr (attrName "redcard") $ centralCardWidget $ ps^.programResources.blueCard10x15
                           , cardWidget $ theCardsOnTable^.red._2
                           ]
                         , [ cardWidget $ theCardsOnTable^.yellow._1
                           , withAttr (attrName "yellowcard") $ centralCardWidget $ ps^.programResources.redCard10x15
                           , cardWidget $ theCardsOnTable^.yellow._2
                           ]
                         , [ cardWidget $ theCardsOnTable^.blue._1
                           , withAttr (attrName "bluecard") $ centralCardWidget $ ps^.programResources.yellowCard10x15
                           , cardWidget $ theCardsOnTable^.blue._2
                           ]
                         , [ cardWidget $ theCardsOnTable^.purple._1
                           , withAttr (attrName "purplecard") $ centralCardWidget $ ps^.programResources.purpleCard10x15
                           , cardWidget $ theCardsOnTable^.purple._2
                           ]
                         ]

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

