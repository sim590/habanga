
{-|
  Module      : Cards
  Description : Logique et types de données des cartes de jeu.
  Copyright   : (c) Simon Désaulniers, 2024
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  Ce module définit et expose les différents types de données utilisés
  pour représenter une carte de jeu et les opérations effectuées
  exclusivement sur elles.

-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Cards where

import Data.Data
import Data.Default

import Control.Lens

import Codec.Serialise

import GHC.Generics

data Color = Red | Yellow | Blue | Purple
  deriving (Eq, Ord, Generic, Data, Read)

data Card  = Card { _value :: Int
                  , _color :: Maybe Color
                  }
                  deriving (Eq, Ord, Generic, Data)
makeLenses ''Card

instance Serialise Color
instance Serialise Card

instance Show Color where
  show Red    = "Rouge"
  show Yellow = "Jaune"
  show Blue   = "Bleu"
  show Purple = "Mauve"

instance Default Card where
  def = Card 0 Nothing

instance Show Card where
  show card = colorString ++ "(" ++ show (card^.value) ++ ")"
    where
      colorString = maybe "Grey" show (card ^. color)

startingRangesCardList :: [Card]
startingRangesCardList = map (`Card` Nothing) [1, 3, 5, 7, 12, 14, 16, 18]

unshuffledDeck :: [Card]
unshuffledDeck = map (`Card` Just Blue)   [1..18]
              ++ map (`Card` Just Yellow) [1..18]
              ++ map (`Card` Just Red)    [1..18]
              ++ map (`Card` Just Purple) [1..18]

{-| Détermine si une carte peut s'insérer ou non dans un intervalle
   donné.

   Ceci est réalisé selon la couleur et la valeur de la carte qui doit
   être comprise dans l'intervalle (bornes exclues).
-}
fits :: Card -> Maybe Color -> (Int, Int) -> Bool
fits (Card v0 col0) col1 (v1, v2)
  | col0 /= col1 = False
  | otherwise    = min v1 v2 < v0 && v0 < max v1 v2

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

