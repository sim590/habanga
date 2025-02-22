
{-|
  Module      : Game
  Description : Types de données utiles aux mécaniques de jeu.
  Copyright   : (c) Simon Désaulniers, 2024
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

-}

{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Default

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
                        | ShareGameSetupFailure String
                        deriving Show
data OnlineGameStatus = AwaitingPlayerTurn
                      | AwaitingOtherPlayerTurn
                      deriving Show
data NetworkRequest = GameAnnounce OnlineGameSettings String
                    | JoinGame GameCode String
                    | GameStart
                    | PlayTurn (Either Card Card)
                    | ResetNetwork
                    deriving Show
data NetworkEvent = Connection
                  | GameStarted
                  deriving Show
data NetworkStatus = AwaitingRequest
                   | AwaitingEvent NetworkEvent
                   | Request NetworkRequest
                   | SharingGameSetup
                   | SetupPhaseDone
                   | GameReadyForInitialization
                   | GameOnGoing OnlineGameStatus
                   | EndingGame
                   | ShuttingDown
                   | NetworkFailure NetworkFailureType
                   deriving Show

data OnlineGameSettings = OnlineGameSettings { _gameCode        :: GameCode
                                             , _numberOfPlayers :: Int
                                             }
                                             deriving Show
makeLenses ''OnlineGameSettings

data GameState = GameState { _cardsOnTable :: CardsOnTable
                           , _deck         :: [Card]
                           , _players      :: [PlayerState]
                           }
               | OnlineGameState { _cardsOnTable          :: CardsOnTable
                                 , _deck                  :: [Card]
                                 , _players               :: [PlayerState]
                                 , _gameTurns             :: Map Word16 (Either Card Card)
                                 , _playersIdentities     :: Map OnlinePlayerID OnlinePlayerName
                                 , _networkStatus         :: NetworkStatus
                                 , _gameSettings          :: OnlineGameSettings
                                 , _gameHostID            :: String
                                 , _turnNumber            :: Word16
                                 , _myID                  :: String
                                 , _myName                :: String
                                 , _myPlayerRank          :: Int
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
  show cs = unlines [ "Red:"
                    , "    " ++ show (cs^.red._1.value)    ++ ", " ++ show (cs^.red._2.value)
                    , "Yellow:"
                    , "    " ++ show (cs^.yellow._1.value) ++ ", " ++ show (cs^.yellow._2.value)
                    , "Blue:"
                    , "    " ++ show (cs^.blue._1.value)   ++ ", " ++ show (cs^.blue._2.value)
                    , "Purple:"
                    , "    " ++ show (cs^.purple._1.value) ++ ", " ++ show (cs^.purple._2.value)
                    ]

instance Show GameState where
  show gs@(GameState {}) = unlines [ "Deck:"
                                   , "    " ++ show (gs^.deck)
                                   , "Players: "
                                   ]
                           ++ "\n"
                           ++ unlines (map (("    "++) . show) (gs^.players))
                           ++ "\n"
                           ++ unlines [ "Cards on table:"
                                      , unlines $ map ("    "++) (lines (show $ gs^.cardsOnTable))
                                      ]
  show gs@(OnlineGameState {}) = show (GameState (gs^.cardsOnTable) (gs^.deck) (gs^.players))
                                 ++ unlines [ "GameTurns:"
                                            , "    " <> show (Map.toList $ gs ^. gameTurns)
                                            , "Player IDs:"
                                            , "    " <> show (Map.toList (gs^.playersIdentities))
                                            , "NetworkStatus: "  <> show (gs^?!networkStatus)
                                            , "GameSettings:"
                                            , "    " <> " GameCode:        " <> gs^.gameSettings.gameCode
                                            , "    " <> " NumberOfPlayers: " <> show (gs^?!gameSettings.numberOfPlayers)
                                            , "GameHostID:   " <> gs ^. gameHostID
                                            , "MyID:         " <> gs ^. myID
                                            , "MyName:       " <> gs ^. myName
                                            , "MyPlayerRank: " <> show (gs ^?! myPlayerRank)
                                            ]

_MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ :: Int
_MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ = 6

defaultOnlineGameState :: GameState
defaultOnlineGameState = OnlineGameState { _cardsOnTable      = def
                                         , _deck              = []
                                         , _players           = []
                                         , _gameTurns         = Map.empty
                                         , _playersIdentities = mempty
                                         , _networkStatus     = AwaitingRequest
                                         , _gameSettings      = OnlineGameSettings [] 0
                                         , _gameHostID        = ""
                                         , _turnNumber        = 0
                                         , _myID              = ""
                                         , _myName            = ""
                                         , _myPlayerRank      = 0
                                         }

splitAtMissingTurn :: GameState -> Map Word16 (Either Card Card) -> ([(Word16, Either Card Card)], [(Word16, Either Card Card)])
splitAtMissingTurn gs gameTurns' = (consecutivePlayerTurns', rest)
  where
    rest = map snd $ dropWhile (uncurry (==)) $ zip (Map.toList gameTurns') consecutivePlayerTurns'
    consecutivePlayerTurns' = consecutivePlayerTurns gs gameTurns'

consecutivePlayerTurns :: GameState -> Map Word16 (Either Card Card) -> [(Word16, Either Card Card)]
consecutivePlayerTurns gs gameTurns' = map fst $ takeWhile turnIsSubsequent $ zip (Map.toList gameTurns') [gs^?!turnNumber..]
  where
    turnIsSubsequent ((t',_), t) = t' == t + 1

{-| Lentille (Lens' s GameState)

   Ceci permet d'interagir avec GameState dans (MonadState s).
-}
gameStateLens :: (GameStated s, Functor f)
              => (GameState -> f GameState) -> s -> f s --
gameStateLens g s = fmap (setGameState s) (g $ getGameState s)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

