{-# LANGUAGE OverloadedStrings #-}
import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (fromException, handle)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar, threadDelay)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS

import qualified Network.WebSockets as WS

data Game = Game { count :: Int }
type ServerState = V.Vector Game

main :: IO ()
main = do
    state <- newMVar V.empty
    WS.runServer "0.0.0.0" 8088 $ application state

application state pending = do
    conn <- WS.acceptRequest pending
    putStrLn "Connection accepted!!!"
    index <- modifyMVar state $ \games -> do
        let newGamePosition = V.length games
        let newGames = V.snoc games (Game { count = 0 })
        return (newGames, newGamePosition)
    putStrLn ("New game with index: " ++ (show index))
    handle catchDisconnect $ forever $ do
        putStrLn "Sending message"
        WS.sendTextData conn ("Test!" :: Text)
        threadDelay 1000000
  where
    catchDisconnect e = case fromException e of
        Just WS.ConnectionClosed -> putStrLn "Someone disconnected"
        _ -> return ()
