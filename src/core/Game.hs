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

module Game ( initializeIO
            , initialize
            , winner
            , processPlayerTurnAction
            , RangeBoundary (..)
            ) where

import Data.Either.Extra
import qualified Data.List as List

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Control.Lens

import System.Random

import Random
import Cards
import GameState

data RangeBoundary = LeftBoundary | RightBoundary

initializeIO
  :: [String]     -- ^ Les noms des différents joueurs.
  -> IO GameState -- ^ L'état du jeu résutlant
initializeIO playerNames = getStdGen >>= initialize playerNames

{-| Fait l'initialisation de l'état du jeu.

  Ceci génère l'état initial de la table de jeu ainsi que du paquet de
  cartes en produisant des permutations aléatoires des listes par défaut
  de chacun. Ensuite, l'état initial des joueurs est généré en
  performant des tirages de carte depuis le paquet de carte.
-}
initialize
  :: RandomGen gen
  => [String] -- ^ Les noms des différents joueurs.
  -> gen
  -> IO GameState
initialize playerNames gen = do
  let
    theCardsOnTable             = CardsOnTable (startingRangesCardsShuffled !! 0, startingRangesCardsShuffled !! 1)
                                               (startingRangesCardsShuffled !! 2, startingRangesCardsShuffled !! 3)
                                               (startingRangesCardsShuffled !! 4, startingRangesCardsShuffled !! 5)
                                               (startingRangesCardsShuffled !! 6, startingRangesCardsShuffled !! 7)
    startingRangesCardsShuffled = deterministiclyShuffle startingRangesCardList gen
    shuffledDeck                = deterministiclyShuffle unshuffledDeck gen
    initialPlayerStates         = map (\ n -> PlayerState n [] Nothing) playerNames

  let initialGameState = GameState theCardsOnTable shuffledDeck initialPlayerStates
  flip execStateT initialGameState $ replicateM_ (length playerNames) $ do
    drawCards 8
    endPlayerTurn

{-| Exécute toute les actions nécessaires lors de la terminaison du tour
   du joueur.
-}
endPlayerTurn :: (MonadState s m, GameStated s) => m ()
endPlayerTurn = do
  let
    cycleAround (p:others) = others ++ [p]
    cycleAround []         = error "endPlayerTurn: aucun joueur!"
    sortByColor            = List.sortBy (\ c0 c1 -> compare (c0^.color) (c1^.color))
    groupByColor           = List.groupBy (\ c0 c1 -> c0^.color == c1^.color)
    sortByColorThenValues  = concat . over traverse List.sort . groupByColor . sortByColor
  gameStateLens . players . _head . cardsInHand %= sortByColorThenValues
  gameStateLens . players %= cycleAround

{-| Tire une carte du paquet
-}
drawCards
  :: (MonadState s m, GameStated s)
  => Int -- ^ Le nombre de cartes à tirer du paquet.
  -> m ()
drawCards n = do
  deckCards <- use $ gameStateLens . deck
  (gameStateLens . players . _head . cardsInHand) %= (++ take n deckCards)
  gameStateLens . deck %= drop n

{-| Retourne le gagnant de la partie.

   Si l'état courant du jeu admet un gagnant, celui-ci est retourné. Autrement,
   "rien" (Nothing) n'est retourné.
-}
winner :: (MonadState s m, GameStated s) => m (Maybe PlayerState)
winner = do
  thePlayers <- use $ gameStateLens . players
  return $ List.find (\ p -> null (p^.cardsInHand)) thePlayers

{-| Effectue les actions associées au tour d'un joueur.

   Le joueur joue sa carte, doit piger lorsque d'autres joueurs ont des
   cartes en relation à l'interval nouvellement créé par le joueur.

   La fonction retourne le nombre de cartes pigées par le joueur à la fin de son tour.
-}

processPlayerTurnAction
  :: (MonadState s m, GameStated s)
  => Either Card Card -- ^ La carte jouée par le joueur. Le type `Either` nous indique si la carte est jouée à gauche ou
                      --   à droite.
  -> MaybeT m Int
processPlayerTurnAction eitherCard = do
  let
    card = fromEither eitherCard
  thePlayers <- use $ gameStateLens . players
  when (null thePlayers) $ error "processPlayerTurnAction: aucun joueur lors de l'exécution du tour d'un joueur?"

  let lastPlayerCardColor      = (^. color) =<< (last thePlayers ^. lastPlayedCard)
  let currentPlayerCardsInhand = head thePlayers ^. cardsInHand

  guard (card `elem` currentPlayerCardsInhand)

  (gameStateLens . players . _head . cardsInHand) %= List.delete card

  let boundaryLens = case eitherCard of
       Left {}  -> _1
       Right {} -> _2

  let colorLens c = case c^.color of
        Just Red    -> red
        Just Yellow -> yellow
        Just Blue   -> blue
        Just Purple -> purple
        Nothing     -> error "processPlayerTurnAction: la carte d'un des joueurs n'avait pas de couleur."


  gameStateLens . cardsOnTable . colorLens card . boundaryLens .= card

  (Card rangeCard1 _, Card rangeCard2 _) <- use (gameStateLens . cardsOnTable . colorLens card)

  let cardMatchesRange c = fits c (card^.color) (rangeCard1, rangeCard2)
  otherPlayersCards <- use (gameStateLens . players . _tail . traverse . cardsInHand)
  (gameStateLens . players . _tail . traverse . cardsInHand) %= filter (not . cardMatchesRange)

  let numberOfCardsToDraw =  length (filter cardMatchesRange otherPlayersCards)
  drawCards numberOfCardsToDraw
  if lastPlayerCardColor == card^.color then do
    drawCards 1
    return $ numberOfCardsToDraw + 1
  else do
    endPlayerTurn
    return numberOfCardsToDraw

-- vim: set sts=2 ts=2 sw=2 tw=120 et :

