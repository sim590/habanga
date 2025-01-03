{-|
  Module      : Game
  Description : Fonctionnalités/mécaniques principales du jeu.
  Copyright   : (c) Simon Désaulniers, 2024
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  Ce fichier contient les fonctionnalités principales de la logique
  dorsale du jeu. Toute instruction visant à assurer le bon comportement
  des mécanismes de jeu devrait être écrit sous ce module.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game ( initialize
            , winner
            , processPlayerTurnAction
            ) where

import qualified Data.List as List

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Lens

import Cards
import GameState

data RangeBoundary = LeftBoundary | RightBoundary

{-| Fait l'initialisation de l'état du jeu.

  Ceci génère l'état initial de la table de jeu ainsi que du paquet de
  cartes en produisant des permutations aléatoires des listes par défaut
  de chacun. Ensuite, l'état initial des joueurs est généré en
  performant des tirages de carte depuis le paquet de carte.
-}
initialize
  :: [String] -- ^ Les noms des différents joueurs.
  -> IO GameState
initialize playerNames = do
  startingRangesCardsShuffled <- shuffleCards startingRangesCardList
  let theCardsOnTable     = CardsOnTable (startingRangesCardsShuffled !! 0, startingRangesCardsShuffled !! 1)
                                         (startingRangesCardsShuffled !! 2, startingRangesCardsShuffled !! 3)
                                         (startingRangesCardsShuffled !! 4, startingRangesCardsShuffled !! 5)
                                         (startingRangesCardsShuffled !! 6, startingRangesCardsShuffled !! 7)
      initialPlayerStates = map (\ n -> PlayerState n [] Nothing) playerNames
  shuffledDeck <- shuffleCards unshuffledDeck

  let initialGameState = GameState theCardsOnTable shuffledDeck initialPlayerStates
  flip execStateT initialGameState $ replicateM_ (length playerNames) $ do
    drawCards 8
    endPlayerTurn

{-| Exécute toute les actions nécessaires lors de la terminaison du tour
   du joueur.
-}
endPlayerTurn :: Monad m => StateT GameState m ()
endPlayerTurn = players %= cycleAround
  where cycleAround (p:others) = others ++ [p]
        cycleAround [] = error "endPlayerTurn: aucun joueur!"

{-| Tire une carte du paquet
-}
drawCards
  :: Monad m
  => Int -- ^ Le nombre de cartes à tirer du paquet.
  -> StateT GameState m ()
drawCards n = do
  deckCards <- use deck
  (players . _head . cardsInHand) %= (++ take n deckCards)
  deck %= drop n

{-| Retourne le gagnant de la partie.

   Si l'état courant du jeu admet un gagnant, celui-ci est retourné. Autrement,
   "rien" (Nothing) n'est retourné.
-}
winner :: Monad m => StateT GameState m (Maybe PlayerState)
winner = do
  thePlayers <- use players
  return $ List.find (\ p -> null (p^.cardsInHand)) thePlayers

{-| Effectue les actions associées au tour d'un joueur.

   Le joueur joue sa carte, doit piger lorsque d'autres joueurs ont des
   cartes en relation à l'interval nouvellement créé par le joueur.
-}
processPlayerTurnAction
  :: Monad m
  => Card          -- ^ La carte jouée par le joueur.
  -> RangeBoundary -- ^ Le côté où placer la carte du joueur (gauche/droite) selon la couleur de
                   --   celle-ci.
  -> MaybeT (StateT GameState m) ()
processPlayerTurnAction card side = do
  thePlayers <- use players
  when (null thePlayers) $ error "processPlayerTurnAction: aucun joueur lors de l'exécution du tour d'un joueur?"

  let lastPlayerCardColor      = (^. color) =<< (last thePlayers ^. lastPlayedCard)
  let currentPlayerName        = head thePlayers ^. name
  let currentPlayerCardsInhand = head thePlayers ^. cardsInHand

  guard (card `elem` currentPlayerCardsInhand)

  (players . _head . cardsInHand) %= List.delete card

  let boundaryLens = case side of
                       LeftBoundary  -> _1
                       RightBoundary -> _2

  let colorLens c = case c^.color of
                      Just Red    -> red
                      Just Yellow -> yellow
                      Just Blue   -> blue
                      Just Purple -> purple
                      Nothing     -> error "processPlayerTurnAction: la carte d'un des joueurs n'avait pas de couleur."


  cardsOnTable . colorLens card . boundaryLens . value .= card^.value

  boundaries <- use (cardsOnTable . colorLens card)

  let otherPlayersPredicate p = (p^.name) /= currentPlayerName
      cardMatchesRange        = flip fits boundaries
  otherPlayersCards <- use (players . traverse . filtered otherPlayersPredicate . cardsInHand)
  (players . traverse . filtered otherPlayersPredicate . cardsInHand) %= filter (not . cardMatchesRange)

  let numberOfCardsToDraw = length $ filter cardMatchesRange otherPlayersCards
  lift $ do
    drawCards numberOfCardsToDraw
    when (lastPlayerCardColor == card^.color) $ drawCards 1
    endPlayerTurn

-- vim: set sts=2 ts=2 sw=2 tw=120 et :

