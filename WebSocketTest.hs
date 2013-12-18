{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Control.Exception (Exception, fromException, handle, SomeException)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, threadDelay, withMVar)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS

import qualified Network.WebSockets as WS

data Player = Player { playerId :: Int
                     , playerConnection :: WS.Connection
                     }

data Game = Game { players :: V.Vector Player
                 , maxPlayerIndex :: Int
                 }

type ServerState = V.Vector Game

main :: IO ()
main = do
    state <- newMVar V.empty
    WS.runServer "0.0.0.0" 8088 $ application state

newGame :: MVar ServerState -> WS.Connection -> IO (Int, Int)
newGame state connection = modifyMVar state $ \games -> do
    let size = V.length games
    let index = 0
    if (size > index)
        then do
            let game = games V.! index
            let newPlayer = makePlayer ((maxPlayerIndex game) + 1)
            putStrLn ("Adding player (id = " ++ (show . playerId $ newPlayer) ++ ") to game with index: " ++ (show index))
            return (games V.// [(index, addPlayer game newPlayer)], (index, playerId newPlayer))
        else do
            putStrLn ("New game with index: " ++ (show index))
            return (games `V.snoc` makeGame, (index, 0))
  where
    addPlayer game player = game { players = (players game) `V.snoc` player
                          , maxPlayerIndex = (maxPlayerIndex game) + 1 }
    makeGame = Game { players = V.singleton (makePlayer 0)
                    , maxPlayerIndex = 0 }
    makePlayer playerId = Player { playerId = playerId
                                 , playerConnection = connection
                                 }

application state pending = do
    conn <- WS.acceptRequest pending
    (gameIndex, pId) <- newGame state conn
    do
        -- get state of current game
        game <- fmap (V.! gameIndex) (readMVar state)
        -- tell new player about other players
        V.mapM_ (\p -> handle (catchDisconnect gameIndex pId) $ sendNewConnection (playerId p) conn) (players game)
        -- tell other players about new player
        V.mapM_ (\p -> handle (catchDisconnect gameIndex (playerId p)) $ sendNewConnection pId (playerConnection p)) (players game)
    handle (catchDisconnect gameIndex pId) $ forever $ do
        sendMessage ((show gameIndex) ++ ", " ++ (show pId) ++ ": Test!") conn
        threadDelay 1000000
  where
    catchDisconnect :: Int -> Int -> SomeException -> IO ()
    catchDisconnect gameIndex pId e = do
        putStrLn ((show gameIndex) ++ ", " ++ (show pId) ++ ": Disconnected")
        removePlayer gameIndex pId
        game <- fmap (V.! gameIndex) (readMVar state)
        V.mapM_ (sendPlayerDisconnect gameIndex pId) (players game)
    removePlayer gameIndex pId = modifyMVar_ state $ \games -> do
        let game = games V.! gameIndex
        let newPlayers = V.filter (\player -> (playerId player) /= pId) (players game)
        return (games V.// [(gameIndex, game { players = newPlayers })])
    sendPlayerDisconnect gameIndex disconnectedIndex player =
        handle (catchDisconnect gameIndex (playerId player)) $ do
            sendDisconnect disconnectedIndex (playerConnection player)

sendNewConnection pId conn = do
    WS.sendTextData conn ("Connection:" `T.append` (T.pack (show pId)))

sendDisconnect pId conn =
    WS.sendTextData conn ("Disconnect:" `T.append` (T.pack (show pId)))

sendMessage message conn =
    WS.sendTextData conn ("Message:" `T.append` (T.pack message))
