
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
               ) where

import GHC.Generics

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

import GameState

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

newNetworkStatusIfNotFail :: NetworkStatus -> GameState -> NetworkStatus
newNetworkStatusIfNotFail ns gs = let currentNetworkStatus = gs ^?! networkStatus in case currentNetworkStatus of
  NetworkFailure {} -> currentNetworkStatus
  _                 -> ns

shareGameSetup :: TVar GameState -> DhtRunnerM Dht ()
shareGameSetup gsTV = liftIO (readTVarIO gsTV) >>= \ gs -> do
  gcHash <- liftIO $ unDht $ infoHashFromString $ gs ^. gameSettings . gameCode
  let
    packet               = HabangaPacket { _senderID = gs ^. myID
                                         , _content  = GameSetup $ gs ^. playersIdentities
                                         }
    gameJoinRequestValue = InputValue { _valueData     = BS.toStrict $ serialise packet
                                      , _valueUserType = _GAME_SETUP_UTYPE_
                                      }
    onDone False gs' = gs' & networkStatus .~ newNetworkStatusIfNotFail failure gs'
      where
        failure = NetworkFailure (ShareGameSetupFailure "network: échec de l'envoi d'une requête pour partager la config de la partie.")
    onDone _ gs'     = gs'
    doneCb success  = atomically $ modifyTVar gsTV $ onDone success
  liftIO $ atomically $ modifyTVar gsTV $ \ gs' -> gs' & networkStatus .~ newNetworkStatusIfNotFail SetupPhaseDone gs'
  void $ DhtRunner.put gcHash gameJoinRequestValue doneCb False

gameJoinRequestCb :: String -> TVar GameState -> ValueCallback
gameJoinRequestCb _    _    (InputValue {})             _    = error $ opendhtWrongValueCtorError "InputValue"
gameJoinRequestCb _    _    (MetaValue {})              _    = error $ opendhtWrongValueCtorError "MetaValue"
gameJoinRequestCb _    _    (StoredValue {})            True = return True
gameJoinRequestCb myId gsTV (StoredValue d _ _ _ utype) False
  | utype == _GAME_JOIN_REQUEST_ACCEPTED_UTYPE_ = deserialiseAndTreatPacket
  | utype == _GAME_SETUP_UTYPE_                 = deserialiseAndTreatPacket
  | otherwise                                   = return True
  where
    deserialiseAndTreatPacket = do
      eitherHabangaPacketOrFail <- try $ return $ deserialise $ BS.fromStrict d
      case eitherHabangaPacketOrFail of
        Left (DeserialiseFailure {}) -> return True
        Right habangaPacket          -> atomically $ stateTVar gsTV $ treatPacket habangaPacket
    treatPacket (HabangaPacket sId (GameSetup playersIds)) gs = (True, gs')
      where
        sId'                 = take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ sId
        myId'                = take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ myId
        currentNetworkStatus = gs ^?! networkStatus
        gs'                  = gs
                                  & playersIdentities              .~ playersIds
                                  & gameHostID                     .~ sId'
                                  & gameSettings . numberOfPlayers .~ length playersIds
                                  & networkStatus                  .~ networkStatus'
        networkStatus'
          | not (Map.member myId' playersIds) = NetworkFailure $ GameJoinRequestFailure ourIdNotFoundFailureMsg
          | not (Map.member sId' playersIds)  = NetworkFailure $ GameJoinRequestFailure hostIdNotFoundFairureMsg
          | otherwise                         = case currentNetworkStatus of
            SetupPhaseDone -> currentNetworkStatus
            _              -> newNetworkStatusIfNotFail SetupPhaseDone gs
    treatPacket (HabangaPacket sId GameJoinRequestAccepted) gs = (True, gsWithGameHostID)
      where
        sId'             = take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ sId
        gsWithGameHostID = gs
                              & gameHostID    .~ sId'
                              & networkStatus .~ newNetworkStatusIfNotFail (AwaitingEvent GameStarted) gs
    treatPacket _ gs = (True, gs)
    ourIdNotFoundFailureMsg  = "network: on a reçu un paquet GameSetup, mais notre ID n'était pas dans la liste."
    hostIdNotFoundFairureMsg = "network: on a reçu un paquet GameSetup, mais l'ID de l'hôte n'était pas dans la liste."


requestToJoinGame :: GameCode -> String -> TVar GameState -> DhtRunnerM Dht ()
requestToJoinGame gc playerName gsTV = liftIO (readTVarIO gsTV) >>= \ initialGameState -> do
  gcHash <- liftIO $ unDht $ infoHashFromString gc
  let
    packet               = HabangaPacket { _senderID = initialGameState ^. myID
                                         , _content  = GameJoinRequest playerName
                                         }
    gameJoinRequestValue = InputValue { _valueData     = BS.toStrict $ serialise packet
                                      , _valueUserType = _GAME_JOIN_REQUEST_UTYPE_
                                      }
    onDone False gs = gs & networkStatus .~ newNetworkStatusIfNotFail failure gs
      where
        failure = NetworkFailure (GameJoinRequestFailure "network: échec de l'envoi d'une requête pour joindre la partie.")
    onDone _ gs     = gs
    doneCb success  = atomically $ modifyTVar gsTV $ onDone success
  liftIO $ atomically $ modifyTVar gsTV $ \ gs -> gs & networkStatus .~ newNetworkStatusIfNotFail (AwaitingEvent Connection) gs
  void $ DhtRunner.put gcHash gameJoinRequestValue doneCb False
  void $ DhtRunner.listen gcHash (gameJoinRequestCb (initialGameState ^. myID) gsTV) shutdownCb

gameAnnounceCb :: Int -> TVar GameState -> ValueCallback
gameAnnounceCb _                  _    (InputValue {})             _    = error $ opendhtWrongValueCtorError "InputValue"
gameAnnounceCb _                  _    (MetaValue {})              _    = error $ opendhtWrongValueCtorError "MetaValue"
gameAnnounceCb _                  _    (StoredValue {})            True = return True
gameAnnounceCb maxNumberOfPlayers gsTV (StoredValue d _ _ _ utype) False
  | utype == _GAME_JOIN_REQUEST_UTYPE_ = deserialiseAndTreatPacket
  | otherwise                          = return True
  where
    treatPacket (HabangaPacket sId (GameJoinRequest pName)) gs =
      let sId' = take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ sId
          gs'  = gs
                    & playersIdentities %~ Map.insert sId' pName
                    & networkStatus     .~ networkStatus'
          networkStatus'
            | not stillHasSpaceForOtherPlayers = newNetworkStatusIfNotFail SharingGameSetup gs
            | otherwise                        = gs ^?! networkStatus
          stillHasSpaceForOtherPlayers = length (gs'^.playersIdentities) < maxNumberOfPlayers
       in case Map.lookup sId' (gs^.playersIdentities) of
            Just _  -> (True, gs)
            Nothing -> (stillHasSpaceForOtherPlayers, gs')
    treatPacket _ gs = (True, gs)

    deserialiseAndTreatPacket = do
      eitherHabangaPacketOrFail <- try $ return $ deserialise $ BS.fromStrict d
      case eitherHabangaPacketOrFail of
        Left (DeserialiseFailure {}) -> return True
        Right habangaPacket          -> atomically $ stateTVar gsTV $ \ gs ->
          if length (gs^.playersIdentities) < maxNumberOfPlayers then treatPacket habangaPacket gs
                                                                 else (False, gs)

announceGame :: OnlineGameSettings -> TVar GameState -> DhtRunnerM Dht OpToken
announceGame (OnlineGameSettings gc maxNumberOfPlayers) gsTV = liftIO (readTVarIO gsTV) >>= \ initialGameState -> do
  gcHash <- liftIO $ unDht $ infoHashFromString gc
  let
    packet            = HabangaPacket { _senderID   = initialGameState ^. myID
                                      , _content    = GameAnnouncement
                                      }
    gameAnnounceValue = InputValue { _valueData     = BS.toStrict $ serialise packet
                                   , _valueUserType = _GAME_ANNOUNCEMENT_UTYPE_
                                   }
    onDone False gs = gs & networkStatus .~ newNetworkStatusIfNotFail failure gs
      where
        failure = NetworkFailure (GameAnnouncementFailure "network: échec de l'envoi du paquet d'annonce de la partie.")
    onDone _ gs     = gs
    doneCb success  = atomically $ modifyTVar gsTV $ onDone success
  liftIO $ atomically $ modifyTVar gsTV $ \ gs -> gs & networkStatus .~ newNetworkStatusIfNotFail (AwaitingEvent Connection) gs
  void $ DhtRunner.put gcHash gameAnnounceValue doneCb True
  DhtRunner.listen gcHash (gameAnnounceCb maxNumberOfPlayers gsTV) shutdownCb

initializeDHT :: DhtRunnerConfig -> DhtRunnerM Dht ()
initializeDHT dhtRconf = do
  DhtRunner.runConfig 0 dhtRconf
  when (all isSpace $ dhtRconf^.proxyServer) $
    DhtRunner.bootstrap _DEFAULT_BOOTSTRAP_ADDR_ _DEFAULT_BOOTSTRAP_PORT_

handleNetworkStatus :: TVar GameState -> NetworkStatus -> DhtRunnerM Dht Bool
handleNetworkStatus _ ShuttingDown = return False
handleNetworkStatus gsTV status    = handleNS status >> return True
  where
    clearPendingDhtOps = do
      gs <- liftIO $ readTVarIO gsTV
      gcHash <- liftIO $ unDht $ infoHashFromString $ gs ^. gameSettings . gameCode
      clearPermanentPutRequests gcHash
      clearListenRequests
    handleNS (Request (JoinGame gc myname)) = do
      liftIO $ atomically $ modifyTVar gsTV $ \ gs' -> gs' & gameSettings.gameCode .~ gc
                                                           & myName                .~ myname
      requestToJoinGame gc myname gsTV
    handleNS (Request (GameAnnounce theGameSettings myname)) = do
      liftIO $ atomically $ modifyTVar gsTV $ \ gs' -> gs' & gameSettings      .~ theGameSettings
                                                           & gameHostID        .~ gs' ^. myID
                                                           & myName            .~ myname
                                                           & playersIdentities .~ Map.fromList [(gs'^.myID, myname)]
      void $ announceGame theGameSettings gsTV
    handleNS SharingGameSetup   = shareGameSetup gsTV
    handleNS SetupPhaseDone = do
      clearPendingDhtOps
      liftIO $ atomically $ modifyTVar gsTV $ networkStatus .~ GameInitialization
    handleNS (Request ResetNetwork) = do
      clearPendingDhtOps
      liftIO $ atomically $ modifyTVar gsTV $ \ gs -> defaultOnlineGameState
        & networkStatus .~ AwaitingRequest
        & myID          .~ gs ^. myID
        & myName        .~ gs ^. myName
    handleNS _ = return ()

loop :: (MonadIO m, MonadReader (TVar GameState) m) => DhtRunnerConfig -> m ()
loop dhtRconf = ask >>= \ gsTV -> liftIO $ readTVarIO gsTV >>= \ case
  OnlineGameState {} -> runDhtRunnerM shutdownCb $ do
    let
      innerLoop False = return ()
      innerLoop True  = do
        gs <- liftIO $ do
          threadDelay _MAIN_LOOP_THREAD_SLEEP_TIME_
          readTVarIO gsTV
        b  <- handleNetworkStatus gsTV (gs^?!networkStatus)
        innerLoop b
    initializeDHT dhtRconf
    myHash <- DhtRunner.getNodeIdHash
    liftIO $ atomically $ modifyTVar gsTV (myID .~ take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ (show myHash))
    innerLoop True
  _ -> error "Network.loop: l'état du jeu passé n'était pas construit par OnlineGameState.."

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

