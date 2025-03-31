
{-|
  Module       : MainMenu
  Description  : Interface texte de l'écran du menu
  Copyright    : (c) Simon Désaulniers, 2025
  License      : GPL-3

  Maintainer   : sim.desaulniers@gmail.com
  Contributors : Simon Désaulniers
                 Xeno Kappel
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module MainMenu ( event
                , attrs
                , widget
                ) where

import qualified Data.Text as Text
import Data.Default
import Data.Maybe
import qualified Data.Map as Map

import Control.Lens
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.STM

import Brick.Util
import Brick.AttrMap
import Brick.Forms
import Brick.Focus ( focusSetCurrent
                   )
import qualified Brick.Types as T
import Brick.Types ( Widget
                   , nestEventM
                   , nestEventM'
                   )
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.Core ( Padding (Pad)
                          , padBottom
                          , padLeft
                          , hLimit
                          , vLimit
                          , vLimitPercent
                          , withAttr
                          , withBorderStyle
                          , str
                          , vBox
                          , hBox
                          , fill
                          , (<+>)
                          )

import qualified Graphics.Vty as V

import OpenDHT.Types
import OpenDHT.InfoHash

import ProgramState
import Widgets

import Game
import Network (forkFinallyWithMvar)
import qualified Network
import qualified BrickNetworkBridge
import qualified BrickNetworkBridge as BNB
import NetworkState (NetworkState)
import qualified NetworkState as NS

type MenuButtonActionPairList = [(NamedButton AppFocus, T.EventM AppFocus ProgramState ())]

attrs :: [(AttrName, V.Attr)]
attrs = [ (E.editAttr,        V.white `on` V.black)
        , (E.editFocusedAttr, V.black `on` V.yellow)
        ]

goUp :: (MonadState s m, Ord b, Num b) => ASetter s s b b -> m ()
goUp menuIndexLens = menuIndexLens %= max 0 . subtract 1

goDown :: (MonadState s m, Foldable t) => t a -> ASetter s s Int Int -> m ()
goDown menuButtons menuIndexLens = menuIndexLens %= min (length menuButtons - 1) . (+ 1)

selectEntry :: MenuButtonActionPairList -> Traversal' ProgramState Int -> T.EventM AppFocus ProgramState ()
selectEntry menuButtons menuIndexLens = do
  mi <- preuse menuIndexLens
  let maction = mi >>= \ i -> over traverse snd menuButtons ^? ix i
  fromMaybe (return ()) maction

resetNetwork :: MonadIO m =>  TVar NetworkState -> m ()
resetNetwork nsTV = Network.requestNetwork nsTV NS.ResetNetwork

goBackOrQuit :: T.EventM AppFocus ProgramState ()
goBackOrQuit = use (mainMenuState.submenu) >>= \ case
  Just (GameInitialization _)         -> mainMenuState.submenu .= Nothing
  Just (OnlineGameSubMenu _)          -> mainMenuState.submenu .= Nothing
  Just (OnlineGameInitialization _ _) -> mainMenuState.submenu .= Just (OnlineGameSubMenu 0)
  Just (OnlineLobby _)                -> mainMenuState.submenu .= Just (OnlineGameSubMenu 0)
  _                                   -> M.halt

withNetwork :: TVar NetworkState -> T.EventM AppFocus ProgramState ()
withNetwork nsTV = get >>= \ ps -> unless (isJust (ps ^. networkMV)) $ do
  netChan <- liftIO newTChanIO
  nMV     <- liftIO $ forkFinallyWithMvar $ runReaderT (Network.loop def) (NS.NetworkStateChannelData nsTV netChan)
  bnbMV   <- liftIO $ do
    forkFinallyWithMvar $ BrickNetworkBridge.loop netChan (ps ^?! brickEventChannel . _Just)
  networkMV            .= Just nMV
  brickNetworkBridgeMV .= Just bnbMV

startGame :: [String] -> T.EventM AppFocus ProgramState ()
startGame playerList = do
  -- FIXME: il faudra utiliser initialize avec un générateur basé sur le code de la partie.
  gameState <~ liftIO (initializeIO playerList)
  currentFocus %= focusSetCurrent (Game Nothing)

createOnlineGame :: TVar NetworkState -> Text.Text -> Int -> T.EventM AppFocus ProgramState ()
createOnlineGame nsTV playerName numberOfPlayers' = withNetwork nsTV >> do
  rHash <- liftIO $ unDht randomInfoHash
  let
    gc              = take NS._GAME_CODE_LENGTH_ $ show rHash
    playerNameStr   = Text.unpack playerName
    theGameSettings = NS.OnlineGameSettings gc numberOfPlayers'
  Network.requestNetwork nsTV $ NS.GameAnnounce theGameSettings playerNameStr

joinOnlineGame :: TVar NetworkState -> Text.Text -> Text.Text -> T.EventM AppFocus ProgramState ()
joinOnlineGame nsTV playerName gc = withNetwork nsTV >> do
  let
    gc'         = Text.unpack gc
    playerName' = Text.unpack playerName
  Network.requestNetwork nsTV $ NS.JoinGame gc' playerName'

event :: TVar NetworkState -> T.BrickEvent AppFocus BNB.NetworkBrickEvent -> T.EventM AppFocus ProgramState ()
event _ (T.AppEvent (BNB.NetworkBrickUpdate ns)) = networkState .= ns
event nsTV ev = do
  let
    buttonMenuEvents :: MenuButtonActionPairList -> Traversal' ProgramState Int -> T.BrickEvent AppFocus BNB.NetworkBrickEvent -> T.EventM AppFocus ProgramState ()
    buttonMenuEvents menuButtons menuIndexLens (T.VtyEvent (V.EvKey V.KEnter      [])) = selectEntry menuButtons menuIndexLens
    buttonMenuEvents menuButtons menuIndexLens (T.VtyEvent (V.EvKey V.KDown       [])) = goDown menuButtons menuIndexLens
    buttonMenuEvents menuButtons menuIndexLens (T.VtyEvent (V.EvKey (V.KChar 'j') [])) = goDown menuButtons menuIndexLens
    buttonMenuEvents _           menuIndexLens (T.VtyEvent (V.EvKey V.KUp         [])) = goUp menuIndexLens
    buttonMenuEvents _           menuIndexLens (T.VtyEvent (V.EvKey (V.KChar 'k') [])) = goUp menuIndexLens
    buttonMenuEvents _           _             (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = goBackOrQuit
    buttonMenuEvents _           _             _                                       = return ()

    formEvents _ (T.VtyEvent (V.EvKey V.KEsc [])) = goBackOrQuit

    formEvents (GameInitialization f) (T.VtyEvent (V.EvKey (V.KChar 'g') [V.MCtrl])) = do
      (_, gameInfo) <- nestEventM f $ gets formState
      mainMenuState.submenu .= Nothing
      let playerNames = filter (not . null) $ map (Text.unpack . Text.strip) $ Text.lines $ gameInfo ^. playerNamesField
      unless (null playerNames) $ startGame playerNames

    formEvents (GameInitialization f) _ = do
      f' <- nestEventM' f (handleFormEvent ev)
      mainMenuState . submenu . _Just . gameForm .= f'

    formEvents (OnlineGameInitialization f pRole) (T.VtyEvent (V.EvKey (V.KChar 'g') [V.MCtrl])) = do
      (_, onlineGameInfo) <- nestEventM f $ gets formState
      let
        playerName = onlineGameInfo ^. myPlayerName
        validName  = not $ Text.null playerName
        nameField  = MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormMyNameField)
      mainMenuState . submenu . _Just . gameForm %= setFieldValid validName nameField
      case pRole of
        Host -> do
          let
            numberOfPlayers'     = onlineGameInfo ^. numberOfPlayers
            validNumberOfPlayers = numberOfPlayers' > 1 && numberOfPlayers' <= 6
            numberOfPlayersField = MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormNumberOfPlayersField)
            validFields          = validName && validNumberOfPlayers
          mainMenuState . submenu . _Just . gameForm %= setFieldValid validNumberOfPlayers numberOfPlayersField
          when validFields $ do
            createOnlineGame nsTV playerName numberOfPlayers'
            mainMenuState . submenu .= Just (OnlineLobby Host)
        OtherPlayer -> do
          let
            gc            = onlineGameInfo ^. gameCode
            validCode     = not $ Text.null gc
            validFields   = validName && validCode
            gameCodeField = MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormGameCodeField)
          mainMenuState . submenu . _Just . gameForm %= setFieldValid validCode gameCodeField
          when validFields $ do
            joinOnlineGame nsTV playerName gc
            mainMenuState . submenu .= Just (OnlineLobby OtherPlayer)

    formEvents (OnlineGameInitialization f _) _ = nestEventM' f (handleFormEvent ev) >>= (mainMenuState . submenu . _Just . onlineGameForm .=)

    formEvents _ _ = error "MainMenu.event.formEvents: sous-menu invalide!"

    onlineLobbyEvents (T.VtyEvent (V.EvKey V.KEsc []))        = resetNetwork nsTV >> goBackOrQuit
    onlineLobbyEvents (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = resetNetwork nsTV >> goBackOrQuit
    onlineLobbyEvents _                                       = return ()

  use (mainMenuState.submenu) >>= \ case
    Just s@(GameInitialization _)         -> formEvents s ev
    Just s@(OnlineGameInitialization _ _) -> formEvents s ev
    Just (OnlineGameSubMenu _)            -> buttonMenuEvents onlineGameMenuButtons (mainMenuState.submenu._Just.onlineGameMenuIndex) ev
    Just (OnlineLobby _)                  -> onlineLobbyEvents ev
    _                                     -> buttonMenuEvents mainMenuButtons (mainMenuState.mainMenuIndex) ev

mainMenuButtons :: MenuButtonActionPairList
mainMenuButtons = [ (button "Jouer",          gameInitializationFormAction)
                  , (button "Jouer en ligne", onlineGameMenuAction        )
                  , (button "Options",        return ()                   )
                  , (button "Quitter",        M.halt                      )
                  ]
  where
    gameInitializationFormAction = do
      mainMenuState.submenu .= Just (GameInitialization (mkGameInitializationForm def))
      currentFocus          %= focusSetCurrent (MainMenu (GameInitializationForm GameInitializationFormPlayerNamesField))
    onlineGameMenuAction = do
      mainMenuState.submenu .= Just (OnlineGameSubMenu 0)
      currentFocus          %= focusSetCurrent OnlineGameMenu

onlineGameMenuButtons :: MenuButtonActionPairList
onlineGameMenuButtons = [ (button "Créer une partie",   onlineGameCreationAction)
                        , (button "Joindre une partie", onlineGameJoinAction    )
                        , (button "Annuler",            goBackOrQuit            )
                        ]
  where
    onlineGameCreationAction = do
      mainMenuState.submenu .= let role = Host in Just (OnlineGameInitialization (mkOnlineGameInitializationForm role def) role)
      currentFocus          %= focusSetCurrent (MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormMyNameField))
    onlineGameJoinAction = do
      mainMenuState.submenu .= let role = OtherPlayer in Just (OnlineGameInitialization (mkOnlineGameInitializationForm role def) role)
      currentFocus          %= focusSetCurrent (MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormMyNameField))

titleWidth :: ProgramState -> Int
titleWidth ps = length $ head $ lines (ps^.programResources.menuGameTitle)

mkGameInitializationForm :: GameInitializationInfo -> Form GameInitializationInfo e AppFocus
mkGameInitializationForm =
    let label s w    = padLeft (Pad 1) $ padBottom (Pad 1) $ vLimit 2 (hLimit 20 $ str s <+> fill ' ') <+> w
        mMaxNumNames = Just _HABANGA_MAX_PLAYER_COUNT_
        focusedItem  = MainMenu (GameInitializationForm GameInitializationFormPlayerNamesField)
    in newForm [ label "Nom des joueurs" @@= B.border
                                         @@= editTextField playerNamesField focusedItem mMaxNumNames
               ]

mkOnlineGameInitializationForm :: OnlineRole -> OnlineGameInitializationInfo -> Form OnlineGameInitializationInfo e AppFocus
mkOnlineGameInitializationForm pRole =
  let label s w             = padLeft (Pad 1) $ padBottom (Pad 1) $ vLimit 2 (hLimit 25 $ str s <+> fill ' ') <+> w
      myNameField           = MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormMyNameField)
      numberOfPlayersField  = MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormNumberOfPlayersField)
      gameCodeField         = MainMenu (OnlineGameInitializationForm OnlineGameInitializationFormGameCodeField)
      playerNameLine        = [ label "Votre nom"               @@= editTextField myPlayerName myNameField (Just 1)        ]
      formLines Host        = [ label "Nombre de joueurs (2-6)" @@= editShowableField numberOfPlayers numberOfPlayersField ]
      formLines OtherPlayer = [ label "Le code de la partie"    @@= editTextField gameCode gameCodeField  (Just 1)         ]
   in newForm $ playerNameLine <> formLines pRole

gameInitializationSubMenu :: ProgramState -> [Widget AppFocus]
gameInitializationSubMenu ps =
  let
    submenuWidget = B.borderWithLabel (str "Configuration du jeu") $ hLimit (titleWidth ps + 4) $ vLimit 25 $ vBox contentWidget
    contentWidget = [ renderForm ((ps ^. mainMenuState . submenu) ^?! _Just . gameForm)
                    , C.hCenter $ str "Ctrl-g pour valider."
                    ]
  in case ps^.mainMenuState.submenu of
    Just (GameInitialization _)       -> [C.centerLayer submenuWidget]
    _                                 -> []

onlineGameInitializationSubMenu :: ProgramState -> [Widget AppFocus]
onlineGameInitializationSubMenu ps =
  let
    windowTitleStr = str "Configuration du jeu (multijoueur)"
    submenuWidget  = B.borderWithLabel windowTitleStr $ hLimit (titleWidth ps + 4) $ vLimit 25 $ vBox contentWidget
    contentWidget  = [ renderForm ((ps ^. mainMenuState . submenu) ^?! _Just . onlineGameForm)
                     , C.hCenter $ str "Ctrl-g pour valider."
                     ]
   in case ps ^. mainMenuState . submenu of
     Just (OnlineGameInitialization _ _) -> [C.centerLayer submenuWidget]
     _                                   -> []

onlineGameSubMenu :: ProgramState -> [Widget AppFocus]
onlineGameSubMenu ps =
  let
    windowTitleStr = str "Jeu en ligne"
    submenuWidget  = B.borderWithLabel windowTitleStr $ hLimit (titleWidth ps + 4) $ vLimit 25 $ vBox contentWidget
    contentWidget  = [buttonMenu onlineGameMenuButtons (mainMenuState.submenu._Just.onlineGameMenuIndex) ps]
   in case ps ^. mainMenuState . submenu of
     Just (OnlineGameSubMenu _) -> [C.centerLayer submenuWidget]
     _                          -> []

onlineLobby :: ProgramState -> [Widget AppFocus]
onlineLobby ps =
  let
    numberOfPlayersConnected = length $ ps ^. networkState . NS.playersIdentities
    expectedNumberOfPlayers  = ps ^. networkState . NS.gameSettings . NS.numberOfPlayers
    gc                       = ps ^. networkState . NS.gameSettings . NS.gameCode
    windowTitleStr Host      = str $ "En attente des joueurs (" <> show numberOfPlayersConnected <> "/" <> show expectedNumberOfPlayers <>")"
    windowTitleStr _         = str "En attente de connexion"
    playersInfo              = Map.toList (ps ^. networkState . NS.playersIdentities)
    playersNameWidgets       = map (\ (pid,  pname) -> C.hCenter $ str $ pname <> " " <> "(" <> pid <> ")" ) playersInfo
    playersConnectionWidget  = playersNameWidgets <> [ C.hCenter $ str "..." ]
    gameCodeWidget           = C.hCenter $ str $ "Code de la partie: " <> gc
    contentWidget            = C.center $ vBox [ vBox $  [ fill ' ' ] <> playersConnectionWidget <> [ fill ' ', B.hBorder ]
                                               , vLimitPercent 10 gameCodeWidget
                                               ]
    submenuWidget role       = B.borderWithLabel (windowTitleStr role) $ hLimit (titleWidth ps + 4) $ vLimit 10 contentWidget
   in case ps ^. mainMenuState . submenu of
     Just (OnlineLobby role) -> [C.centerLayer $ submenuWidget role]
     _                       -> []

buttonMenu :: MenuButtonActionPairList -> Traversal' ProgramState Int -> ProgramState -> Widget AppFocus
buttonMenu menuButtons menuIndexLens ps = vBox $ map C.center $ appendArgsToButtons (over traverse fst menuButtons) 25
  where
    -- Cette fonction est un peu moins évidente, mais elle assure que les
    -- indices des boutons sont bien attribués et que la largeur de ceux-ci
    -- est la même pour tous.
    appendArgsToButtons bs width = fst (foldl (\ (l, i) f -> (l++[f i], i+1)) ([], 0) bs) <*> [width]
                                                                                          <*> [ps^?!menuIndexLens]
                                                                                          <*> [attrName "selectedAttr"]

widget :: ProgramState -> [Widget AppFocus]
widget ps = subMenus <> [ hBox [ leftPanel
                               , hLimit (titleWidth ps) middlePanel
                               , rightPanel
                               ]
                        ]
  where
    subMenus         = gameInitializationSubMenu ps <> onlineGameSubMenu ps <> onlineGameInitializationSubMenu ps <> onlineLobby ps
    sidePanelStyle a = C.center . withBorderStyle BS.unicodeRounded . B.border . withAttr (attrName a) . str
    leftPanel        = sidePanelStyle "bluecard"   $ ps^.programResources.blueCard35x53
    rightPanel       = sidePanelStyle "purplecard" $ ps^.programResources.purpleCard35x53

    middlePanel      = vBox [ C.hCenter $ str $ ps^.programResources.menuGameTitle
                            , C.hCenter $ B.borderWithLabel (str "Menu principal") $ hLimit (titleWidth ps) $ C.center menuOptions
                            ]
    menuOptions      = buttonMenu mainMenuButtons (mainMenuState.mainMenuIndex) ps

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

