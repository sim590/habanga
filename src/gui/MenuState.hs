{-# LANGUAGE TemplateHaskell #-}

module MenuState
    ( MenuState(..)
    , menuOption
    , menuOpen
    , MenuAction(..)
    , menuActions
    , menuTexts
    , menuCount
    ) where

import Control.Lens
import Data.Default

data MenuState = MenuState
  { _menuOption    :: Int
  , _menuOpen      :: Bool
  } deriving (Show, Eq)

makeLenses ''MenuState

instance Default MenuState where
  def = MenuState 0 True

data MenuAction = NewGame | Continue | Options | QuitGame deriving (Show, Eq)

menuTexts :: [String]
menuTexts = ["Jouer", "Continuer", "Options", "Quitter"]

menuCount :: Int
menuCount = length menuTexts

menuActions :: Int -> MenuAction
menuActions 0 = NewGame
menuActions 1 = Continue
menuActions 2 = Options
menuActions _ = QuitGame

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

