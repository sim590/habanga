
module OnlineGame where

import Data.Maybe
import qualified Data.List as List
import qualified Data.Map as Map

import Numeric

import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Lens

import System.Random

import OpenDHT.Types
import OpenDHT.InfoHash

import Game
import GameState
import NetworkState
import Random

randomGeneratorFromNetworkState :: NetworkState -> StdGen
randomGeneratorFromNetworkState ns = mkStdGen (fst $ head $ readHex $ ns ^. gameSettings . gameCode)

shuffledPlayerNames :: NetworkState -> StdGen -> [String]
shuffledPlayerNames ns = deterministiclyShuffle sortedPlayerNames
  where
    sortedPlayerNames = List.sort $ Map.elems $ ns ^. playersIdentities

resetNetwork :: (MonadIO m) => Maybe (TChan NetworkRequest) -> m ()
resetNetwork Nothing        = return ()
resetNetwork (Just reqChan) = liftIO $ atomically $ writeTChan reqChan ResetNetwork

createGame :: MonadIO m => TChan NetworkRequest -> String -> Int -> m ()
createGame reqChan playerName numberOfPlayers' = do
  rHash <- liftIO $ unDht randomInfoHash
  let
    gc              = take _GAME_CODE_LENGTH_ $ show rHash
    playerNameStr   = playerName
    theGameSettings = OnlineGameSettings gc numberOfPlayers'
  liftIO $ atomically $ writeTChan reqChan $ GameAnnounce theGameSettings playerNameStr

joinGame :: MonadIO m => TChan NetworkRequest -> String -> String -> m ()
joinGame reqChan playerName gc = liftIO $ atomically $ writeTChan reqChan $ JoinGame gc playerName

startGame :: MonadIO m => NetworkState -> TChan NetworkRequest -> m GameState
startGame ns reqChan = do
  let
    playerName  = ns ^. myName
    gen         = randomGeneratorFromNetworkState ns
    shuffPNames = shuffledPlayerNames ns gen
  gs <- liftIO $ initialize shuffPNames gen
  let myRank = fromJust $ playerRank playerName gs
  liftIO $ atomically $ writeTChan reqChan $ GameStart myRank
  return gs

myCurrentPosInPlayerList :: NetworkState -> Int
myCurrentPosInPlayerList ns = (myRank - tn) `mod` n
  where
    myRank = ns ^. myPlayerRank
    tn     = fromIntegral $ ns ^. turnNumber
    n      = ns ^. gameSettings . numberOfPlayers

isMyTurn :: NetworkState -> Bool
isMyTurn ns = myCurrentPosInPlayerList ns == 0

consumeConsecutivePlayerTurns :: NetworkState -> ([GameTurn], NetworkState)
consumeConsecutivePlayerTurns ns = (turnsToConsume, ns')
  where
    turnsToConsume = consecutivePlayerTurns ns (ns ^. gameTurns)
    ns'            = ns & turnNumber +~ fromIntegral (length turnsToConsume)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

