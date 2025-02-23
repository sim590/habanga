
{-|
  Module      : Game
  Description : Types de données utiles aux mécaniques de jeu.
  Copyright   : (c) Simon Désaulniers, 2024
  License     : GPL-3

  Maintainer  : sim.desaulniers@gmail.com

-}

{-# LANGUAGE TemplateHaskell #-}

module GameState where

import Data.Default

import Control.Lens
import Control.Monad.IO.Class
import Control.Concurrent.STM

import Cards
import NetworkState

data CardsOnTable = CardsOnTable { _red    :: (Card, Card)
                                 , _yellow :: (Card, Card)
                                 , _blue   :: (Card, Card)
                                 , _purple :: (Card, Card)
                                 }
makeLenses ''CardsOnTable

instance Default CardsOnTable where
  def = CardsOnTable def def def def

data PlayerState = PlayerState { _name           :: String
                               , _cardsInHand    :: [Card]
                               , _lastPlayedCard :: Maybe Card
                               }
makeLenses ''PlayerState

instance Show PlayerState where
  show p = (p^.name) ++ ": " ++ show (p^.cardsInHand)

data GameState = GameState { _cardsOnTable :: CardsOnTable
                           , _deck         :: [Card]
                           , _players      :: [PlayerState]
                           }
               | OnlineGameState { _cardsOnTable :: CardsOnTable
                                 , _deck         :: [Card]
                                 , _players      :: [PlayerState]
                                 , _networkState :: TVar NetworkState
                                 }
makeLenses ''GameState

class GameStated a where
  -- TODO: de façon à utiliser TVar
  -- getGameState :: a -> IO GameState
  -- setGameState :: a -> GameState -> IO a
  getGameState :: a -> GameState
  setGameState :: a -> GameState -> a

instance GameStated GameState where
  getGameState = id
  setGameState _ gs = gs

instance Default GameState where
  def = GameState def [] []

instance Show CardsOnTable where
  show cs = unlines [ "Red:"
                    , "    " ++ show (cs^.red._1.value)    ++ ", " ++ show (cs^.red._2.value)
                    , "Yellow:"
                    , "    " ++ show (cs^.yellow._1.value) ++ ", " ++ show (cs^.yellow._2.value)
                    , "Blue:"
                    , "    " ++ show (cs^.blue._1.value)   ++ ", " ++ show (cs^.blue._2.value)
                    , "Purple:"
                    , "    " ++ show (cs^.purple._1.value) ++ ", " ++ show (cs^.purple._2.value)
                    ]

instance Show GameState where
  show gs = unlines [ "Deck:"
                    , "    " ++ show (gs^.deck)
                    , "Players: "
                    ]
            ++ "\n"
            ++ unlines (map (("    "++) . show) (gs^.players))
            ++ "\n"
            ++ unlines [ "Cards on table:"
                      , unlines $ map ("    "++) (lines (show $ gs^.cardsOnTable))
                      ]

defaultOnlineGameState :: MonadIO m => m GameState
defaultOnlineGameState = liftIO (newTVarIO def) >>= \ nsTV ->
  return OnlineGameState { _cardsOnTable = def ^. cardsOnTable
                         , _deck         = def ^. deck
                         , _players      = def ^. players
                         , _networkState = nsTV
                         }

{-| Lentille (Lens' s GameState)

   Ceci permet d'interagir avec GameState dans (MonadState s).
-}
gameStateLens :: (GameStated s, Functor f)
              => (GameState -> f GameState) -> s -> f s --
gameStateLens g s = fmap (setGameState s) (g $ getGameState s)

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

