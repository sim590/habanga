
module Random where

import Data.Functor

import Control.Monad.State

import System.Random


shuffle :: [a] -> IO [a]
shuffle as = getStdGen <&> deterministiclyShuffle as

deterministiclyShuffle :: RandomGen gen => [a] -> gen -> [a]
deterministiclyShuffle es gen = flip evalState (es, gen) $ replicateM (length es) $ do
  (l, gen') <- get
  let (s, gen'') = random gen'
      r          = s `mod` length l
  case splitAt r l of
    (beg, c:end) -> do
      put (beg ++ end, gen'')
      return c
    (_, []) -> error "shuffle: la liste était vide. Ceci n'aurait pas dû se produire..."

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

