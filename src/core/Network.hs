
{-|
  Module      : Network
  Description : Couche réseau d'Habanga
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

  Ce module définit les différents types de données et
  fonctions nécessaires à l'échange en multijoueur avec
  plusieurs joueurs.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Network ( loop
               , forkFinallyWithMvar
               ) where

import GHC.Generics

import Data.Word
import Data.Default
import Data.Data
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS

import Codec.Serialise

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Lens

import OpenDHT.Value
import OpenDHT.Types
import OpenDHT.InfoHash
import OpenDHT.DhtRunner ( DhtRunnerM
                         , DhtRunnerConfig
                         , ValueCallback
                         , ShutdownCallback
                         , runDhtRunnerM
                         , proxyServer
                         , OpToken
                         )
import qualified OpenDHT.DhtRunner as DhtRunner

import Cards
import NetworkState

forkFinallyWithMvar :: IO () -> IO (MVar ())
forkFinallyWithMvar io = do
  mvar <- newEmptyMVar
  void $ forkFinally io (\ _ -> putMVar mvar ())
  return mvar

-- Le temps de pause de la boucle en microsecondes
_MAIN_LOOP_THREAD_SLEEP_TIME_ :: Int
_MAIN_LOOP_THREAD_SLEEP_TIME_ = 500000

_DEFAULT_BOOTSTRAP_ADDR_ :: String
_DEFAULT_BOOTSTRAP_ADDR_  = "bootstrap.jami.net"

_DEFAULT_BOOTSTRAP_PORT_ :: String
_DEFAULT_BOOTSTRAP_PORT_  = "4222"

opendhtWrongValueCtorError :: String -> String
opendhtWrongValueCtorError = (<>) "Network: la fonction de rappel (listen) a retourné le type de valeur inattendu "

data HabangaPacketContent = GameAnnouncement
                          | GameJoinRequest { _playerName :: String
                                            }
                          | GameJoinRequestAccepted
                          | GameSetup { _playersIdentities :: Map OnlinePlayerID OnlinePlayerName
                                      }
                          | PlayerTurn { _playedCard :: Either Card Card
                                       , _turnNumber :: Word16
                                       }
  deriving (Generic, Data, Show)
data HabangaPacket = HabangaPacket { _senderID   :: String
                                   , _content    :: HabangaPacketContent
                                   }
  deriving Generic

instance Serialise HabangaPacketContent
instance Serialise HabangaPacket

_GAME_ANNOUNCEMENT_UTYPE_ :: String
_GAME_ANNOUNCEMENT_UTYPE_ = show $ toConstr GameAnnouncement

_GAME_JOIN_REQUEST_UTYPE_ :: String
_GAME_JOIN_REQUEST_UTYPE_ = show $ toConstr $ GameJoinRequest ""

_GAME_JOIN_REQUEST_ACCEPTED_UTYPE_ :: String
_GAME_JOIN_REQUEST_ACCEPTED_UTYPE_ = show $ toConstr GameJoinRequestAccepted

_GAME_SETUP_UTYPE_ :: String
_GAME_SETUP_UTYPE_ = show $ toConstr $ GameSetup mempty

_PLAYER_TURN_UTYPE_ :: String
_PLAYER_TURN_UTYPE_ = show $ toConstr $ PlayerTurn (Left def) 0

-- TODO:
shutdownCb :: ShutdownCallback
shutdownCb = return ()

clearPermanentPutRequests :: InfoHash -> DhtRunnerM Dht ()
clearPermanentPutRequests h = do
  values <- DhtRunner.getPermanentMetaValues
  forM_ values $ \ v -> DhtRunner.cancelPut h (_valueId v)

clearListenRequests :: DhtRunnerM Dht ()
clearListenRequests = do
  tokenMap <- DhtRunner.getListenTokens
  forM_ (Map.toList tokenMap) $ \ (h, tokens) ->
    forM_ tokens $ \ t ->
      runMaybeT (DhtRunner.cancelListen h t)

clearPendingDhtOps :: NetworkState -> DhtRunnerM Dht ()
clearPendingDhtOps ns = do
  gcHash <- liftIO $ unDht $ infoHashFromString $ ns ^. gameSettings . gameCode
  clearPermanentPutRequests gcHash
  clearListenRequests

newNetworkStatusSafe :: NetworkStatus -> NetworkState -> NetworkStatus
newNetworkStatusSafe nStatus ns = let currentNetworkStatus = ns ^. status in case currentNetworkStatus of
  NetworkFailure {} -> currentNetworkStatus
  ShuttingDown      -> currentNetworkStatus
  _                 -> nStatus

atomicallyHandleStateUpdate :: TVar NetworkState -> NetworkTwoWayChannel -> STM a -> IO a
atomicallyHandleStateUpdate nsTV (NetworkTwoWayChannel _ updateChan) stateAction = atomically $ do
  a  <- stateAction
  ns <- readTVar nsTV
  writeTChan updateChan (NetworkChannelUpdate ns)
  return a


playMyTurn :: Word16 -> Either Card Card -> TVar NetworkState -> NetworkTwoWayChannel -> DhtRunnerM Dht ()
playMyTurn tn card nsTV n2wChan@(NetworkTwoWayChannel _ _) = liftIO (readTVarIO nsTV) >>= \ ns -> do
  gcHash <- liftIO $ unDht $ infoHashFromString $ ns ^. gameSettings . gameCode
  let
    packet          = HabangaPacket { _senderID = ns ^. myID
                                    , _content  = PlayerTurn card tn
                                    }
    playerTurnValue = InputValue { _valueData     = BS.toStrict $ serialise packet
                                 , _valueUserType = _PLAYER_TURN_UTYPE_
                                 }
    onDone False ns' = ns' & status .~ newNetworkStatusSafe failure ns'
      where
        failure = NetworkFailure (ShareGameSetupFailure "network: échec de l'envoi mon jeu pour ce tour.")
    onDone _ ns' = ns'
    doneCb success  = atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ onDone success
  liftIO $ atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ \ ns' -> ns' & status .~ newNetworkStatusSafe (GameOnGoing AwaitingOtherPlayerTurn) ns'
  void $ DhtRunner.put gcHash playerTurnValue doneCb False

gameOnGoingCb :: GameCode -> TVar NetworkState -> NetworkTwoWayChannel -> ValueCallback
gameOnGoingCb _ _    _                                  (InputValue {})             _    = error $ opendhtWrongValueCtorError "InputValue"
gameOnGoingCb _ _    _                                  (MetaValue {})              _    = error $ opendhtWrongValueCtorError "MetaValue"
gameOnGoingCb _ _    _                                  (StoredValue {})            True = return True
gameOnGoingCb gc nsTV n2wChan@(NetworkTwoWayChannel _ _) (StoredValue d _ _ _ utype) False
  | utype == _PLAYER_TURN_UTYPE_ = deserialiseAndTreatPacket
  | otherwise                    = return True
  where
    deserialiseAndTreatPacket = do
      eitherHabangaPacketOrFail <- try $ return $ deserialise $ BS.fromStrict d
      case eitherHabangaPacketOrFail of
        Left (DeserialiseFailure {}) -> return True
        Right habangaPacket          -> atomicallyHandleStateUpdate nsTV n2wChan $ stateTVar nsTV $ treatPacket habangaPacket
    treatPacket (HabangaPacket sId (PlayerTurn card tn)) ns
      | sId == ns ^. myID                   = (True, ns)
      | gc /= ns ^. gameSettings . gameCode = (True, ns)
      | otherwise                           = (True, ns')
      where
        ns'            = ns & gameTurns .~ gameTurns'
                            & status    .~ newNetworkStatusSafe networkStatus' ns
        gameTurns'     = Map.insert tn card (ns^.gameTurns)
        lastTurnNumber = fromIntegral $ fst $ last $ consecutivePlayerTurns ns gameTurns'
        n              = ns ^. gameSettings . numberOfPlayers
        isOurTurn      = lastTurnNumber `mod` n == (ns ^. myPlayerRank - 1) `mod` n
        networkStatus'
          | isOurTurn = GameOnGoing AwaitingPlayerTurn
          | otherwise = ns ^. status
    treatPacket _ ns = (True, ns)

shareGameSetup :: TVar NetworkState -> NetworkTwoWayChannel -> DhtRunnerM Dht ()
shareGameSetup nsTV n2wChan@(NetworkTwoWayChannel _ _) = liftIO (readTVarIO nsTV) >>= \ ns -> do
  gcHash <- liftIO $ unDht $ infoHashFromString $ ns ^. gameSettings . gameCode
  let
    packet               = HabangaPacket { _senderID = ns ^. myID
                                         , _content  = GameSetup $ ns ^. playersIdentities
                                         }
    gameJoinRequestValue = InputValue { _valueData     = BS.toStrict $ serialise packet
                                      , _valueUserType = _GAME_SETUP_UTYPE_
                                      }
    onDone False ns' = ns' & status .~ newNetworkStatusSafe failure ns'
      where
        failure = NetworkFailure (ShareGameSetupFailure "network: échec de l'envoi d'une requête pour partager la config de la partie.")
    onDone _ ns'     = ns'
    doneCb success  = atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ onDone success
  liftIO $ atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ \ ns' -> ns' & status .~ newNetworkStatusSafe SetupPhaseDone ns'
  void $ DhtRunner.put gcHash gameJoinRequestValue doneCb False

gameJoinRequestCb :: String -> TVar NetworkState -> NetworkTwoWayChannel -> ValueCallback
gameJoinRequestCb _    _    _                                  (InputValue {})             _    = error $ opendhtWrongValueCtorError "InputValue"
gameJoinRequestCb _    _    _                                  (MetaValue {})              _    = error $ opendhtWrongValueCtorError "MetaValue"
gameJoinRequestCb _    _    _                                  (StoredValue {})            True = return True
gameJoinRequestCb myId nsTV n2wChan@(NetworkTwoWayChannel _ _) (StoredValue d _ _ _ utype) False
  | utype == _GAME_JOIN_REQUEST_ACCEPTED_UTYPE_ = deserialiseAndTreatPacket
  | utype == _GAME_SETUP_UTYPE_                 = deserialiseAndTreatPacket
  | otherwise                                   = return True
  where
    deserialiseAndTreatPacket = do
      eitherHabangaPacketOrFail <- try $ return $ deserialise $ BS.fromStrict d
      case eitherHabangaPacketOrFail of
        Left (DeserialiseFailure {}) -> return True
        Right habangaPacket          -> atomicallyHandleStateUpdate nsTV n2wChan $ stateTVar nsTV $ treatPacket habangaPacket
    treatPacket (HabangaPacket sId (GameSetup playersIds)) ns = (True, ns')
      where
        sId'                 = take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ sId
        myId'                = take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ myId
        currentNetworkStatus = ns ^. status
        ns'                  = ns
                                  & playersIdentities              .~ playersIds
                                  & gameHostID                     .~ sId'
                                  & gameSettings . numberOfPlayers .~ length playersIds
                                  & status                         .~ networkStatus'
        networkStatus'
          | not (Map.member myId' playersIds) = NetworkFailure $ GameJoinRequestFailure ourIdNotFoundFailureMsg
          | not (Map.member sId' playersIds)  = NetworkFailure $ GameJoinRequestFailure hostIdNotFoundFairureMsg
          | otherwise                         = case currentNetworkStatus of
            SetupPhaseDone -> currentNetworkStatus
            _              -> newNetworkStatusSafe SetupPhaseDone ns
    treatPacket (HabangaPacket sId GameJoinRequestAccepted) ns = (True, gsWithGameHostID)
      where
        sId'             = take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ sId
        gsWithGameHostID = ns
                              & gameHostID    .~ sId'
                              & status .~ newNetworkStatusSafe (AwaitingEvent GameStarted) ns
    treatPacket _ ns = (True, ns)
    ourIdNotFoundFailureMsg  = "network: on a reçu un paquet GameSetup, mais notre ID n'était pas dans la liste."
    hostIdNotFoundFairureMsg = "network: on a reçu un paquet GameSetup, mais l'ID de l'hôte n'était pas dans la liste."


requestToJoinGame :: GameCode -> String -> TVar NetworkState -> NetworkTwoWayChannel -> DhtRunnerM Dht ()
requestToJoinGame gc playerName nsTV n2wChan@(NetworkTwoWayChannel _ _) = liftIO (readTVarIO nsTV) >>= \ initialGameState -> do
  gcHash <- liftIO $ unDht $ infoHashFromString gc
  let
    packet               = HabangaPacket { _senderID = initialGameState ^. myID
                                         , _content  = GameJoinRequest playerName
                                         }
    gameJoinRequestValue = InputValue { _valueData     = BS.toStrict $ serialise packet
                                      , _valueUserType = _GAME_JOIN_REQUEST_UTYPE_
                                      }
    onDone False ns = ns & status .~ newNetworkStatusSafe failure ns
      where
        failure = NetworkFailure (GameJoinRequestFailure "network: échec de l'envoi d'une requête pour joindre la partie.")
    onDone _ ns     = ns
    doneCb success  = atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ onDone success
  liftIO $ atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ \ ns -> ns & status .~ newNetworkStatusSafe (AwaitingEvent Connection) ns
  void $ DhtRunner.put gcHash gameJoinRequestValue doneCb False
  void $ DhtRunner.listen gcHash (gameJoinRequestCb (initialGameState ^. myID) nsTV n2wChan) (return ())

gameAnnounceCb :: Int -> TVar NetworkState -> NetworkTwoWayChannel -> ValueCallback
gameAnnounceCb _                  _    _                                  (InputValue {})             _    = error $ opendhtWrongValueCtorError "InputValue"
gameAnnounceCb _                  _    _                                  (MetaValue {})              _    = error $ opendhtWrongValueCtorError "MetaValue"
gameAnnounceCb _                  _    _                                  (StoredValue {})            True = return True
gameAnnounceCb maxNumberOfPlayers nsTV n2wChan@(NetworkTwoWayChannel _ _) (StoredValue d _ _ _ utype) False
  | utype == _GAME_JOIN_REQUEST_UTYPE_ = deserialiseAndTreatPacket
  | otherwise                          = return True
  where
    treatPacket (HabangaPacket sId (GameJoinRequest pName)) ns =
      let sId' = take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ sId
          ns'  = ns
                    & playersIdentities %~ Map.insert sId' pName
                    & status            .~ newNetworkStatusSafe networkStatus' ns
          networkStatus'
            | not stillHasSpaceForOtherPlayers = SharingGameSetup
            | otherwise                        = ns ^. status
          stillHasSpaceForOtherPlayers = length (ns'^.playersIdentities) < maxNumberOfPlayers
       in case Map.lookup sId' (ns^.playersIdentities) of
            Just _  -> (True, ns)
            Nothing -> (stillHasSpaceForOtherPlayers, ns')
    treatPacket _ ns = (True, ns)

    deserialiseAndTreatPacket = do
      eitherHabangaPacketOrFail <- try $ return $ deserialise $ BS.fromStrict d
      case eitherHabangaPacketOrFail of
        Left (DeserialiseFailure {}) -> return True
        Right habangaPacket          -> atomicallyHandleStateUpdate nsTV n2wChan $ stateTVar nsTV $ \ ns ->
          if length (ns^.playersIdentities) < maxNumberOfPlayers then treatPacket habangaPacket ns
                                                                 else (False, ns)

announceGame :: OnlineGameSettings -> TVar NetworkState -> NetworkTwoWayChannel -> DhtRunnerM Dht OpToken
announceGame (OnlineGameSettings gc maxNumberOfPlayers) nsTV n2wChan@(NetworkTwoWayChannel _ _) = liftIO (readTVarIO nsTV) >>= \ initialGameState -> do
  gcHash <- liftIO $ unDht $ infoHashFromString gc
  let
    packet            = HabangaPacket { _senderID   = initialGameState ^. myID
                                      , _content    = GameAnnouncement
                                      }
    gameAnnounceValue = InputValue { _valueData     = BS.toStrict $ serialise packet
                                   , _valueUserType = _GAME_ANNOUNCEMENT_UTYPE_
                                   }
    onDone False ns = ns & status .~ newNetworkStatusSafe failure ns
      where
        failure = NetworkFailure (GameAnnouncementFailure "network: échec de l'envoi du paquet d'annonce de la partie.")
    onDone _ ns     = ns
    doneCb success  = atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ onDone success
  liftIO $ atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ \ ns -> ns & status .~ newNetworkStatusSafe (AwaitingEvent Connection) ns
  void $ DhtRunner.put gcHash gameAnnounceValue doneCb False
  DhtRunner.listen gcHash (gameAnnounceCb maxNumberOfPlayers nsTV n2wChan) (return ())

initializeDHT :: DhtRunnerConfig -> DhtRunnerM Dht ()
initializeDHT dhtRconf = do
  DhtRunner.runConfig 0 dhtRconf
  when (all isSpace $ dhtRconf^.proxyServer) $
    DhtRunner.bootstrap _DEFAULT_BOOTSTRAP_ADDR_ _DEFAULT_BOOTSTRAP_PORT_

handleRequest :: TVar NetworkState -> NetworkTwoWayChannel -> DhtRunnerM Dht Bool
handleRequest nsTV n2wChan@(NetworkTwoWayChannel reqChan _) = liftIO (atomically $ tryReadTChan reqChan) >>= \ case
  Nothing  -> return True
  Just req -> do
              liftIO (atomically $ modifyTVar nsTV $ \ ns -> ns & status .~ newNetworkStatusSafe TreatingRequest ns)
              handleReq req
              return True
  where
    handleReq (JoinGame gc myname) = do
      liftIO $ atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ \ ns' -> ns'
        & gameSettings.gameCode .~ gc
        & myName                .~ myname
      requestToJoinGame gc myname nsTV n2wChan
    handleReq (GameAnnounce theGameSettings myname) = do
      liftIO $ atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ \ ns' -> ns'
        & gameSettings      .~ theGameSettings
        & gameHostID        .~ ns' ^. myID
        & myName            .~ myname
        & playersIdentities .~ Map.fromList [(ns'^.myID, myname)]
      void $ announceGame theGameSettings nsTV n2wChan
    handleReq (GameStart myRank) = do
      ns     <- liftIO $ readTVarIO nsTV
      gcHash <- liftIO $ unDht $ infoHashFromString $ ns ^. gameSettings . gameCode
      let
        networkStatus'
          | myRank == 0 = GameOnGoing AwaitingPlayerTurn
          | otherwise   = GameOnGoing AwaitingOtherPlayerTurn
        gc = ns ^. gameSettings . gameCode
      liftIO $ atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ \ ns' -> ns'
        & status .~ newNetworkStatusSafe networkStatus' ns'
        & myPlayerRank  .~ myRank
        & turnNumber    .~ 0
      void $ DhtRunner.listen gcHash (gameOnGoingCb gc nsTV n2wChan) (return ())
    handleReq (PlayTurn card) = do
      ns <- liftIO $ readTVarIO nsTV
      liftIO $ atomically $ modifyTVar nsTV $ turnNumber +~ 1
      playMyTurn (ns ^. turnNumber) card nsTV n2wChan
    handleReq (UpdateTurnNumber tn) = liftIO $ atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ turnNumber .~ tn
    handleReq ResetNetwork = do
      ns <- liftIO $ readTVarIO nsTV
      clearPendingDhtOps ns
      liftIO $ atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ \ ns' -> def
        & status .~ newNetworkStatusSafe AwaitingRequest ns'
        & myID          .~ ns' ^. myID
        & myName        .~ ns' ^. myName
    handleReq Shutdown = liftIO $ atomically $ modifyTVar nsTV $ status .~ ShuttingDown

handleNetworkStatus :: TVar NetworkState -> NetworkTwoWayChannel -> NetworkStatus -> DhtRunnerM Dht Bool
handleNetworkStatus nsTV (NetworkTwoWayChannel _ chan) ShuttingDown = do
  liftIO $ readTVarIO nsTV >>= atomically . writeTChan chan . NetworkChannelUpdate
  return False
handleNetworkStatus nsTV n2wChan nStatus = handleNS nStatus >> handleRequest nsTV n2wChan >> return True
  where
    handleNS SharingGameSetup = shareGameSetup nsTV n2wChan
    handleNS SetupPhaseDone = do
      ns <- liftIO $ readTVarIO nsTV
      clearPendingDhtOps ns
      liftIO $ atomicallyHandleStateUpdate nsTV n2wChan $ modifyTVar nsTV $ \ ns' -> ns' & status .~ newNetworkStatusSafe GameReadyForInitialization ns'
    handleNS _ = return ()

loop :: MonadIO m => DhtRunnerConfig -> NetworkTwoWayChannel -> m ()
loop dhtRconf n2wChan = liftIO $ runDhtRunnerM shutdownCb $ do
  nsTV <- liftIO (newTVarIO def)
  let
    innerLoop False = return ()
    innerLoop True  = do
      ns <- liftIO $ do
        threadDelay _MAIN_LOOP_THREAD_SLEEP_TIME_
        readTVarIO nsTV
      b  <- handleNetworkStatus nsTV n2wChan (ns^.status)
      innerLoop b
  initializeDHT dhtRconf
  myHash <- DhtRunner.getNodeIdHash
  liftIO $ atomically $ modifyTVar nsTV (myID .~ take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ (show myHash))
  innerLoop True

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

