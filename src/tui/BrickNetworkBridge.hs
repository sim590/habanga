
{-|
  Module      : BrickNetworkBridge
  Description : Module de liaison entre la couche réseau et l'interface Bric
  Copyright   : (c) Simon Désaulniers, 2025
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com
-}

module BrickNetworkBridge ( NetworkBrickEvent (..)
                          , loop
                          ) where

import Control.Lens
import Control.Monad.Extra ( whileM )
import Control.Concurrent.STM

import Brick.BChan

import NetworkState

newtype NetworkBrickEvent = NetworkUpdate NetworkState

updateNetworkState :: TChan NetworkChannelUpdate -> Brick.BChan.BChan NetworkBrickEvent -> IO Bool
updateNetworkState netChan brickChan = atomically (readTChan netChan) >>= \ (NetworkChannelUpdate ns) -> case ns^.status of
   ShuttingDown -> return False
   _            -> writeBChan brickChan (NetworkUpdate ns) >> return True

loop :: TChan NetworkChannelUpdate
     -> Brick.BChan.BChan NetworkBrickEvent
     -> IO ()
loop nchan bchan = whileM $ updateNetworkState nchan bchan

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

