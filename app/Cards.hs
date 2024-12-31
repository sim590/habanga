
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

module Cards where

import System.Random

import Control.Monad.State
import Control.Lens

data Color = Red | Yellow | Blue | Purple
  deriving (Eq, Show)

data Card  = Card { _value :: Int
                  , _color :: Maybe Color
                  }
                  deriving Eq
makeLenses ''Card

instance Show Card where
  show card = colorString ++ "(" ++ show (card^.value) ++ ")"
    where
      colorString = maybe "Grey" show (card ^. color)

startingRangesCardList :: [Card]
startingRangesCardList = map (`Card` Nothing) [1, 3, 5, 7, 12, 14, 16, 18]

unshuffledDeck :: [Card]
unshuffledDeck = map (`Card` Just Blue)   [1..18]
              ++ map (`Card` Just Yellow) [1..18]
              ++ map (`Card` Just Blue)   [1..18]
              ++ map (`Card` Just Purple) [1..18]

{-| Détermine si une carte peut s'insérer ou non dans un intervalle
   donné.

   Ceci est réalisé selon la couleur et la valeur de la carte qui doit
   être comprise dans l'intervalle (bornes exclues).
-}
fits :: Card -> (Card, Card) -> Bool
fits (Card v0 col0) (Card v1 col1, Card v2 col2)
  | col1 /= col2 = error "fits: Oops!! Inconsistent card colors on given input interval."
  | col0 /= col1 = False
  | otherwise    = min v1 v2 < v0 && v0 < max v1 v2

shuffleCards :: [Card] -> IO [Card]
shuffleCards cards = flip evalStateT cards $ forM [1..length cards] $ \ _ -> do
  l <- get
  s <- randomIO
  let r            = s `mod` length l
      (beg, c:end) = splitAt r l
  put (beg ++ end)
  return c

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

