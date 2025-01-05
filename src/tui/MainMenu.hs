
{-# LANGUAGE LambdaCase #-}

module MainMenu ( event
                , attrs
                , widget
                ) where

import qualified Data.Text as Text
import Data.Default
import Data.Maybe

import Control.Lens
import Control.Monad.State

import Brick.AttrMap
import Brick.Forms
import qualified Brick.Types as T
import Brick.Types (Widget
                   , nestEventM
                   , nestEventM'
                   )
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core ( Padding (Pad)
                          , padBottom
                          , padLeft
                          , hLimit
                          , vLimit
                          , withAttr
                          , withBorderStyle
                          , str
                          , vBox
                          , hBox
                          , fill
                          , (<+>)
                          )

import qualified Graphics.Vty as V

import ProgramState
import Widgets

import Game

attrs :: [(AttrName, V.Attr)]
attrs = [ ]

goUp :: T.EventM AppFocus ProgramState ()
goUp = use (mainMenuState . submenu) >>= \ case
   Just (GameInitialization _) -> return ()
   _                           -> mainMenuState.mainMenuIndex %= max 0 . subtract 1

goDown :: T.EventM AppFocus ProgramState ()
goDown = use (mainMenuState . submenu) >>= \ case
   Just (GameInitialization _) -> return ()
   _                           -> mainMenuState.mainMenuIndex %= min (length buttons - 1) . (+ 1)

selectEntry :: T.EventM AppFocus ProgramState ()
selectEntry = do
  i <- use $ mainMenuState.mainMenuIndex
  let maction = over traverse snd buttons ^? ix i
  fromMaybe (return ()) maction

goBackOrQuit :: T.EventM AppFocus ProgramState ()
goBackOrQuit = use (mainMenuState.submenu) >>= \ case
  Just (GameInitialization _) -> mainMenuState.submenu .= Nothing
  _                           -> M.halt

startGame :: [String] -> T.EventM AppFocus ProgramState ()
startGame playerList = do
  gameState     <~ liftIO (initialize playerList)
  currentScreen .= Just Game

event :: T.BrickEvent AppFocus () -> T.EventM AppFocus ProgramState ()
event ev = do
  let
    mainEvents (T.VtyEvent (V.EvKey V.KEnter      [])) = selectEntry
    mainEvents (T.VtyEvent (V.EvKey V.KDown       [])) = goDown
    mainEvents (T.VtyEvent (V.EvKey (V.KChar 'j') [])) = goDown
    mainEvents (T.VtyEvent (V.EvKey V.KUp         [])) = goUp
    mainEvents (T.VtyEvent (V.EvKey (V.KChar 'k') [])) = goUp
    mainEvents (T.VtyEvent (V.EvKey V.KEsc        [])) = goBackOrQuit
    mainEvents (T.VtyEvent (V.EvKey (V.KChar 'q') [])) = goBackOrQuit
    mainEvents _                                       = return ()
  use (mainMenuState.submenu) >>= \ case
    Just (GameInitialization f) -> case ev of
      (T.VtyEvent (V.EvKey V.KEsc []))               -> goBackOrQuit
      (T.VtyEvent (V.EvKey (V.KChar 'g') [V.MCtrl])) -> do
        (_, gameInfo) <- nestEventM f $ gets formState
        mainMenuState.submenu .= Nothing
        let playerNames = filter (not . null) $ map (Text.unpack . Text.strip) $ Text.lines $ gameInfo ^. playerNamesField
        unless (null playerNames) $ startGame playerNames
      _                                         -> do
        f' <- nestEventM' f (handleFormEvent ev)
        mainMenuState . submenu . _Just . gameForm .= f'
    _ -> mainEvents ev

buttons :: [(Int -> Int -> Int -> AttrName -> Widget AppFocus, T.EventM AppFocus ProgramState ())]
buttons = [ (button "Jouer",   mainMenuState.submenu .= Just (GameInitialization (mkGameInitializationForm def)))
          , (button "Options", return ()                                                                        )
          , (button "Quitter", M.halt                                                                           )
          ]

titleWidth :: ProgramState -> Int
titleWidth ps = length $ head $ lines (ps^.programResources.menuGameTitle)

mkGameInitializationForm :: GameInitializationInfo -> Form GameInitializationInfo e AppFocus
mkGameInitializationForm =
    let label s w = padLeft (Pad 1) $ padBottom (Pad 1) $ vLimit 2 (hLimit 20 $ str s <+> fill ' ') <+> w
    in newForm [ label "Nom des joueurs" @@= B.border
                                         @@= editTextField playerNamesField GameInitializationFormPlayerNamesField (Just _HABANGA_MAX_PLAYER_COUNT_)
               ]

gameInitializationSubMenu :: ProgramState -> [Widget AppFocus]
gameInitializationSubMenu ps =
  let
    submenuWidget = B.borderWithLabel (str "Configuration du jeu") $ hLimit (titleWidth ps + 4) $ vLimit 25 $ vBox contentWidget
    contentWidget = [ renderForm ((ps ^. mainMenuState . submenu) ^?! _Just . gameForm)
                    , C.hCenter $ str "Ctrl-g pour valider."
                    ]
  in case ps^.mainMenuState.submenu of
    Just (GameInitialization _) -> [C.centerLayer submenuWidget]
    _                           -> []

widget :: ProgramState -> [Widget AppFocus]
widget ps = subMenus <> [ hBox [ leftPanel
                               , hLimit (titleWidth ps) middlePanel
                               , rightPanel
                               ]
                        ]
  where
    subMenus         = gameInitializationSubMenu ps
    sidePanelStyle a = C.center . withBorderStyle BS.unicodeRounded . B.border . withAttr (attrName a) . str
    leftPanel        = sidePanelStyle "bluecard"   $ ps^.programResources.blueCard35x53
    rightPanel       = sidePanelStyle "purplecard" $ ps^.programResources.purpleCard35x53

    middlePanel      = vBox [ C.hCenter $ str $ ps^.programResources.menuGameTitle
                            , C.hCenter $ B.borderWithLabel (str "Menu principal") $ hLimit (titleWidth ps) $ C.center menuOptions
                            ]
    menuOptions      = vBox $ map C.center $ appendArgsToButtons (over traverse fst buttons) 25
    -- Cette fonction est un peu moins évidente, mais elle assure que les
    -- indices des boutons sont bien attribués et que la largeur de ceux-ci
    -- est la même pour tous.
    appendArgsToButtons bs width = fst (foldl (\ (l, i) f -> (l++[f i], i+1)) ([], 0) bs) <*> [width]
                                                                                          <*> [ps^.mainMenuState.mainMenuIndex]
                                                                                          <*> [attrName "selectedAttr"]

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

