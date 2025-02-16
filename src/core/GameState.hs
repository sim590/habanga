
{-|
  Module      : Game
  Description : Types de données utiles aux mécaniques de jeu.
  Copyright   : (c) Simon Désaulniers, 2024
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

-}

{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Data.Map (Map)
import Data.Default
import qualified Data.List as List

import Control.Lens

import Cards

data CardsOnTable = CardsOnTable { _red    :: (Card, Card)
                                 , _yellow :: (Card, Card)
                                 , _blue   :: (Card, Card)
                                 , _purple :: (Card, Card)
                                 }
makeLenses ''CardsOnTable

instance Default CardsOnTable where
  def = CardsOnTable def def def def

data PlayerState = PlayerState { _name           :: String
                               , _cardsInHand    :: [Card]
                               , _lastPlayedCard :: Maybe Card
                               }
makeLenses ''PlayerState

instance Show PlayerState where
  show p = (p^.name) ++ ": " ++ show (p^.cardsInHand)

type GameCode = String

type OnlinePlayerID = String
type OnlinePlayerName = String

data NetworkFailureType = GameAnnouncementFailure String
                        | GameJoinRequestFailure String
                        | GameInitialSetupFailure String
data NetworkStatus = Awaiting
                   | AwaitingConnection
                   | AwaitingPlayerTurn
                   | AwaitingOtherPlayerTurn
                   | RequestGameAnnounce
                   | EndingGame
                   | ShuttingDown
                   | NetworkFailure NetworkFailureType

data OnlineGameSettings = OnlineGameSettings { _gameCode        :: GameCode
                                             , _numberOfPlayers :: Int
                                             }
makeLenses ''OnlineGameSettings

data GameState = GameState { _cardsOnTable :: CardsOnTable
                           , _deck         :: [Card]
                           , _players      :: [PlayerState]
                           }
               | OnlineGameState { _cardsOnTable          :: CardsOnTable
                                 , _deck                  :: [Card]
                                 , _players               :: [PlayerState]
                                 , _playersIdentities     :: Map OnlinePlayerID OnlinePlayerName
                                 , _networkStatus         :: NetworkStatus
                                 , _gameSettings          :: OnlineGameSettings
                                 , _myID                  :: String
                                 }
makeLenses ''GameState

class GameStated a where
  -- TODO: de façon à utiliser TVar
  -- getGameState :: a -> IO GameState
  -- setGameState :: a -> GameState -> IO a
  getGameState :: a -> GameState
  setGameState :: a -> GameState -> a

instance GameStated GameState where
  getGameState = id
  setGameState _ gs = gs

instance Default GameState where
  def = GameState def [] []

instance Show CardsOnTable where
  show cs = List.intercalate "\n" [ "Red: "    ++ "\n\t" ++ show (cs^.red._1.value)    ++ ", " ++ show (cs^.red._2.value)
                                  , "Yellow: " ++ "\n\t" ++ show (cs^.yellow._1.value) ++ ", " ++ show (cs^.yellow._2.value)
                                  , "Blue: "   ++ "\n\t" ++ show (cs^.blue._1.value)   ++ ", " ++ show (cs^.blue._2.value)
                                  , "Purple: " ++ "\n\t" ++ show (cs^.purple._1.value) ++ ", " ++ show (cs^.purple._2.value)
                                  ]

instance Show GameState where
  show gs = List.intercalate "\n" [ "Deck: "      ++ "\n" ++ "\t" ++ show (gs^.deck)
                                  , "Players: "
                                  ]
         ++ "\n"
         ++ List.intercalate "\n" (map (("\t"++) . show) (gs^.players))
         ++ "\n"
         ++ List.intercalate "\n" [ "Cards on table:"
                                  , List.intercalate "\n" $ map ("\t"++) (lines (show $ gs^.cardsOnTable))
                                  ]

_MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ :: Int
_MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ = 6

{-| Lentille (Lens' s GameState)

   Ceci permet d'interagir avec GameState dans (MonadState s).
-}
gameStateLens :: (GameStated s, Functor f)
              => (GameState -> f GameState) -> s -> f s --
gameStateLens g s = fmap (setGameState s) (g $ getGameState s)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

