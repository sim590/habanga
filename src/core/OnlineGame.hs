
module OnlineGame where

import qualified Data.List as List
import qualified Data.Map as Map

import Numeric

import Control.Lens

import System.Random

import NetworkState
import Random

randomGeneratorFromNetworkState :: NetworkState -> StdGen
randomGeneratorFromNetworkState ns = mkStdGen (fst $ head $ readHex $ ns ^. gameSettings . gameCode)

shuffledPlayerNames :: NetworkState -> StdGen -> [String]
shuffledPlayerNames ns = deterministiclyShuffle sortedPlayerNames
  where
    sortedPlayerNames = List.sort $ Map.elems $ ns ^. playersIdentities

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

