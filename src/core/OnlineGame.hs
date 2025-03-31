
module OnlineGame where

import qualified Data.List as List
import qualified Data.Map as Map

import Numeric

import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Lens

import System.Random

import OpenDHT.Types
import OpenDHT.InfoHash

import qualified Network
import NetworkState
import Random

randomGeneratorFromNetworkState :: NetworkState -> StdGen
randomGeneratorFromNetworkState ns = mkStdGen (fst $ head $ readHex $ ns ^. gameSettings . gameCode)

shuffledPlayerNames :: NetworkState -> StdGen -> [String]
shuffledPlayerNames ns = deterministiclyShuffle sortedPlayerNames
  where
    sortedPlayerNames = List.sort $ Map.elems $ ns ^. playersIdentities

createGame :: MonadIO m => TVar NetworkState -> String -> Int -> m ()
createGame nsTV playerName numberOfPlayers' = do
  rHash <- liftIO $ unDht randomInfoHash
  let
    gc              = take _GAME_CODE_LENGTH_ $ show rHash
    playerNameStr   = playerName
    theGameSettings = OnlineGameSettings gc numberOfPlayers'
  Network.requestNetwork nsTV $ GameAnnounce theGameSettings playerNameStr

joinGame :: MonadIO m => TVar NetworkState -> String -> String -> m ()
joinGame nsTV playerName gc = Network.requestNetwork nsTV $ JoinGame gc playerName

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

