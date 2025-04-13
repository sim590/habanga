
{-# LANGUAGE TemplateHaskell #-}

module NetworkState where

import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Default

import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan
import Control.Lens

import Cards

type GameCode         = String
type OnlinePlayerID   = String
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
                    | UpdateTurnNumber Word16
                    | ResetNetwork
                    | Shutdown
                    deriving Show
data NetworkEvent = Connection
                  | GameStarted
                  deriving Show
data NetworkStatus = AwaitingRequest
                   | AwaitingEvent NetworkEvent
                   | TreatingRequest
                   | SharingGameSetup
                   | SetupPhaseDone
                   | GameReadyForInitialization
                   | GameOnGoing OnlineGameStatus
                   | GameEnded
                   | ShuttingDown
                   | Offline
                   | NetworkFailure NetworkFailureType
                   deriving Show

data OnlineGameSettings = OnlineGameSettings { _gameCode        :: GameCode
                                             , _numberOfPlayers :: Int
                                             }
                                             deriving Show
makeLenses ''OnlineGameSettings

type GameTurn = (Word16, Either Card Card)

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
  def = NetworkState { _gameTurns         = Map.empty
                     , _playersIdentities = mempty
                     , _status            = Offline
                     , _gameSettings      = OnlineGameSettings [] 0
                     , _gameHostID        = ""
                     , _turnNumber        = 0
                     , _myID              = ""
                     , _myName            = ""
                     , _myPlayerRank      = 0
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
                    , "TurnNumber:   " <> show (ns ^. turnNumber)
                    ]

newtype NetworkChannelUpdate = NetworkChannelUpdate NetworkState

data NetworkTwoWayChannel = NetworkTwoWayChannel { _requestChannel :: TChan NetworkRequest
                                                 , _updateChannel  :: TChan NetworkChannelUpdate
                                                 }
makeLenses ''NetworkTwoWayChannel

_GAME_CODE_LENGTH_ :: Int
_GAME_CODE_LENGTH_ = 6

_MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ :: Int
_MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ = 6

splitAtMissingTurn :: NetworkState -> Map Word16 (Either Card Card) -> ([GameTurn], [GameTurn])
splitAtMissingTurn ns gameTurns' = (consecutivePlayerTurns', rest)
  where
    rest = map snd $ dropWhile (uncurry (==)) $ zip (Map.toList gameTurns') consecutivePlayerTurns'
    consecutivePlayerTurns' = consecutivePlayerTurns ns gameTurns'

consecutivePlayerTurns :: NetworkState -> Map Word16 (Either Card Card) -> [GameTurn]
consecutivePlayerTurns ns gameTurns' = map fst $ takeWhile turnIsSubsequent $ zip gameTurnsFromTurnNumber [ns^.turnNumber..]
  where
    turnIsSubsequent ((t',_), t) = t' == t
    gameTurnsFromTurnNumber      = dropWhile ((< ns ^. turnNumber) . fst) $ Map.toList gameTurns'

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

