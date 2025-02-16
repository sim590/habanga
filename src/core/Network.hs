
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
import qualified Data.Map as Map
import qualified Data.ByteString as BS

import Codec.Serialise

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Trans
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

-- TODO:
shutdownCb :: ShutdownCallback
shutdownCb = return ()

newNetworkStatusIfNotFail :: NetworkStatus -> GameState -> NetworkStatus
newNetworkStatusIfNotFail ns gs = let currentNetworkStatus = gs ^?! networkStatus in case currentNetworkStatus of
  NetworkFailure {} -> currentNetworkStatus
  _                 -> ns

gameAnnounceCb :: Int -> TVar GameState -> ValueCallback
gameAnnounceCb _                  _    (InputValue {})             _    = error $ opendhtWrongValueCtorError "InputValue"
gameAnnounceCb _                  _    (MetaValue {})              _    = error $ opendhtWrongValueCtorError "MetaValue"
gameAnnounceCb _                  _    (StoredValue {})            True = return True
gameAnnounceCb maxNumberOfPlayers gsTV (StoredValue d _ _ _ utype) False
  | utype == _GAME_JOIN_REQUEST_UTYPE_ = do
    let
      treatPacket (HabangaPacket sId (GameJoinRequest pName)) gs =
        let sId'                     = take _MAX_PLAYER_ID_SIZE_TO_CONSIDER_UNIQUE_ sId
            stateWithNewPlayer       = gs & playersIdentities %~ Map.insert sId' pName
            stillSpaceAfterNewPlayer = length (stateWithNewPlayer^.playersIdentities) < maxNumberOfPlayers
         in case Map.lookup sId' (gs^.playersIdentities) of
              Just _  -> (True, gs)
              Nothing -> (stillSpaceAfterNewPlayer, stateWithNewPlayer)
      treatPacket _ gs = (True, gs)

    eitherHabangaPacketOrFail <- try $ return $ deserialise $ BS.fromStrict d
    case eitherHabangaPacketOrFail of
      Left (DeserialiseFailure {}) -> return True
      Right habangaPacket                     -> atomically $ stateTVar gsTV $ \ gs ->
        if length (gs^.playersIdentities) < maxNumberOfPlayers then treatPacket habangaPacket gs
                                                               else (False, gs)
  | otherwise = return True

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
  liftIO $ atomically $ modifyTVar gsTV $ \ gs -> gs & networkStatus .~ newNetworkStatusIfNotFail AwaitingConnection gs
  void $ DhtRunner.put gcHash gameAnnounceValue doneCb True
  DhtRunner.listen gcHash (gameAnnounceCb maxNumberOfPlayers gsTV) shutdownCb

initializeDHT :: DhtRunnerConfig -> DhtRunnerM Dht ()
initializeDHT dhtRconf = do
  DhtRunner.runConfig 0 dhtRconf
  when (all isSpace $ dhtRconf^.proxyServer) $
    DhtRunner.bootstrap _DEFAULT_BOOTSTRAP_ADDR_ _DEFAULT_BOOTSTRAP_PORT_

handleNetworkStatus :: TVar GameState -> NetworkStatus -> DhtRunnerM Dht Bool
handleNetworkStatus _ ShuttingDown            = return False
handleNetworkStatus gsTV status               = handle status >> return True
  where
    handle RequestGameAnnounce = do
      gs <- liftIO $ readTVarIO gsTV
      void $ announceGame (gs^?!gameSettings) gsTV
    handle (NetworkFailure (GameAnnouncementFailure msg)) = undefined -- TODO: annuler tous les puts/listen
    handle _ = return ()

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

