{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module AmqpWorker where

import           Control.Concurrent      (forkIO)
import           Control.Monad.Catch     (SomeException)
import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Function           ((&))
import           Data.Text               (Text, pack)
import           GHC.Generics            (Generic)
import           Network.AMQP.Worker     (Connection, Message (..),
                                          WorkerException, def, fromURI)
import qualified Network.AMQP.Worker     as Worker
import           Network.AMQP.Worker.Key
import           System.IO               (BufferMode (..), hSetBuffering,
                                          stderr, stdout)

data Product = Product { name :: Text, price :: Integer } deriving (Generic, Show, Eq)
instance FromJSON Product
instance ToJSON Product

ordersQueue :: Key Routing Product
ordersQueue = key "orders" & word "new"

initialize :: IO Connection
initialize = do
  conn <- Worker.connect (fromURI "amqp://guest:guest@localhost:5672")
  Worker.bindQueue conn (Worker.direct ordersQueue)
  return conn

frontend :: Connection -> IO ()
frontend conn = do (productName, price) <- return ("a",7)
                   Worker.publish conn ordersQueue Product { name = productName, price = price }
                   putStrLn "Message sent"
  
 
  -- -- publish a message
  -- putStrLn "Publishing a message"
  -- Worker.publish conn newMessages (TMessage $ pack msg)

  -- -- create a worker, the program loops here
  -- _ <- forkIO $ Worker.worker conn def (Worker.direct newMessages) onError (onMessage conn)
  -- _ <- forkIO $ Worker.worker conn def (handleAnyMessages) onError (onMessage conn)
  -- initQueue conn ordersQueue
  -- return conn
