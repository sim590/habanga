
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

module Network ( networkLoop
               ) where

import GHC.Generics

import Data.Char
import qualified Data.ByteString as BS
import Data.Default

import Codec.Serialise

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
                         , runDhtRunnerM
                         , proxyServer
                         )
import qualified OpenDHT.DhtRunner as DhtRunner

import GameState

_DEFAULT_BOOTSTRAP_ADDR_ :: String
_DEFAULT_BOOTSTRAP_ADDR_  = "bootstrap.jami.net"

_DEFAULT_BOOTSTRAP_PORT_ :: String
_DEFAULT_BOOTSTRAP_PORT_  = "4222"

opendhtWrongValueCtorError :: String -> String
opendhtWrongValueCtorError = (<>) "Network: la fonction de rappel (listen) a retourné le type de valeur inattendu "

data HabangaPacketType = GameAnnounce
  deriving (Generic, Show)
data HabangaPacket = HabangaPacket { _senderID   :: String
                                   , _packetType :: HabangaPacketType
                                   }
  deriving Generic

instance Serialise HabangaPacketType
instance Serialise HabangaPacket

listenForPlayerConnectionCb :: ValueCallback
listenForPlayerConnectionCb (InputValue {})  _       = error $ opendhtWrongValueCtorError "InputValue"
listenForPlayerConnectionCb (MetaValue {})   _       = error $ opendhtWrongValueCtorError "MetaValue"
listenForPlayerConnectionCb (StoredValue {}) expired = undefined

announceGame :: GameCode -> TVar GameState -> DhtRunnerM Dht ()
announceGame gc gsTV = do
  myHash <- DhtRunner.getNodeIdHash
  gcHash <- liftIO $ unDht $ infoHashFromString gc
  let
    packet            = HabangaPacket { _senderID   = show myHash
                                      , _packetType = GameAnnounce
                                      }
    gameAnnounceValue = InputValue { _valueData     = BS.toStrict $ serialise packet
                                   , _valueUserType = show GameAnnounce
                                   }
    onDone False gs = gs & networkStatus .~ GameConnectionFail
    onDone True gs  = gs & networkStatus .~ AwaitingConnection
    doneCb success  = atomically $ modifyTVar gsTV $ onDone success
  void $ DhtRunner.put gcHash gameAnnounceValue doneCb True

shutdownCb :: IO ()
shutdownCb  = return ()

initializeDHT :: DhtRunnerConfig -> DhtRunnerM Dht ()
initializeDHT dhtRconf = do
  DhtRunner.runConfig 0 dhtRconf
  when (all isSpace $ dhtRconf^.proxyServer) $
    DhtRunner.bootstrap _DEFAULT_BOOTSTRAP_ADDR_ _DEFAULT_BOOTSTRAP_PORT_

-- TODO: annuler le put permanent quand les joueurs sont tous connectés (usertype=GameAnnounce)
handleNetworkStatus :: NetworkStatus -> DhtRunnerM Dht Bool
handleNetworkStatus ShuttingDown = return False
handleNetworkStatus status       = handle status >> return True
  where
    handle _ = undefined

networkLoop :: (MonadIO m, MonadReader (TVar GameState) m) => GameCode -> DhtRunnerConfig -> m ()
networkLoop gc dhtRconf = ask >>= \ gsTV -> liftIO $ runDhtRunnerM shutdownCb $ do
  let
    loop False = return ()
    loop True  = do
      gs <- liftIO $ do
        threadDelay 500000
        readTVarIO gsTV
      b  <- handleNetworkStatus (gs^?!networkStatus)
      loop b
  initializeDHT dhtRconf
  announceGame gc gsTV
  loop True

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

