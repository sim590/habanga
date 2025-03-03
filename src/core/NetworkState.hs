
{-# LANGUAGE TemplateHaskell #-}

module NetworkState where

import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Default

import Control.Lens

import Cards

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
                    | GameStart Int
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
                   | GameEnded
                   | ShuttingDown
                   | NetworkFailure NetworkFailureType
                   deriving Show

data OnlineGameSettings = OnlineGameSettings { _gameCode        :: GameCode
                                             , _numberOfPlayers :: Int
                                             }
                                             deriving Show
makeLenses ''OnlineGameSettings

data NetworkState = NetworkState { _gameTurns         :: Map Word16 (Either Card Card)
                                 , _playersIdentities :: Map OnlinePlayerID OnlinePlayerName
                                 , _status            :: NetworkStatus
                                 , _gameSettings      :: OnlineGameSettings
                                 , _gameHostID        :: String
                                 , _turnNumber        :: Word16
                                 , _myID              :: String
                                 , _myName            :: String
                                 , _myPlayerRank      :: Int
                                 }
makeLenses ''NetworkState

instance Default NetworkState where
  def = NetworkState { _gameTurns         =  Map.empty
                     , _playersIdentities =  mempty
                     , _status            =  AwaitingRequest
                     , _gameSettings      =  OnlineGameSettings [] 0
                     , _gameHostID        =  ""
                     , _turnNumber        =  0
                     , _myID              =  ""
                     , _myName            =  ""
                     , _myPlayerRank      =  0
                     }

instance Show NetworkState where
  show ns = unlines [ "GameTurns:"
                    , "    " <> show (Map.toList $ ns ^. gameTurns)
                    , "Player IDs:"
                    , "    " <> show (Map.toList (ns ^. playersIdentities))
                    , "NetworkStatus: "  <> show (ns ^. status)
                    , "GameSettings:"
                    , "    " <> " GameCode:        " <> ns ^. gameSettings . gameCode
                    , "    " <> " NumberOfPlayers: " <> show (ns ^. gameSettings . numberOfPlayers)
                    , "GameHostID:   " <> ns ^. gameHostID
                    , "MyID:         " <> ns ^. myID
                    , "MyName:       " <> ns ^. myName
                    , "MyPlayerRank: " <> show (ns ^. myPlayerRank)
                    ]

_MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ :: Int
_MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ = 6

splitAtMissingTurn :: NetworkState -> Map Word16 (Either Card Card) -> ([(Word16, Either Card Card)], [(Word16, Either Card Card)])
splitAtMissingTurn ns gameTurns' = (consecutivePlayerTurns', rest)
  where
    rest = map snd $ dropWhile (uncurry (==)) $ zip (Map.toList gameTurns') consecutivePlayerTurns'
    consecutivePlayerTurns' = consecutivePlayerTurns ns gameTurns'

consecutivePlayerTurns :: NetworkState -> Map Word16 (Either Card Card) -> [(Word16, Either Card Card)]
consecutivePlayerTurns ns gameTurns' = map fst $ takeWhile turnIsSubsequent $ zip (Map.toList gameTurns') [ns^.turnNumber..]
  where
    turnIsSubsequent ((t',_), t) = t' == t + 1

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

