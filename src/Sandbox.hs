module Sandbox where

import Control.DeepSeq
import Control.Monad.Par
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random
import GHC.Conc.Sync

findFactors :: Integer -> [Integer]
findFactors 1 = [1]
findFactors n = let oneFactor = findFactor n 2
                    in oneFactor : (findFactors $ n `div` oneFactor)

findFactor :: Integer -> Integer -> Integer
findFactor n m | n == m         = n
               | n `mod` m == 0 = m
               | otherwise      = findFactor n (m + 1)

findTwoFactorsSeq :: Integer -> Integer -> ([Integer],[Integer])
findTwoFactorsSeq x y = (findFactors x, findFactors y)

findTwoFactors :: Integer -> Integer -> ([Integer],[Integer])
findTwoFactors x y = runPar $ do
  factorsXVar <- spawnP $ findFactors x
  let factorsY = findFactors y
      _        = rnf factorsY
  factorsX <- get factorsXVar
  return (factorsX, factorsY)
concurrentUpdate :: IO ()
concurrentUpdate = do v <- newMVar 10000
                      forkIO $ updateMoney v
                      forkIO $ updateMoney v
                      forkIO $ updateMoney v
                      _ <- getLine
                      return ()

updateMoney :: MVar Integer -> IO ()
updateMoney v = do m <- takeMVar v 
                   putStrLn $ "Updating value, which is " ++ show m
                   putMVar v (m + 500) 

readMoney :: MVar Integer -> IO ()
readMoney v = do m <- readMVar v
                 putStrLn $ "The Current value is " ++ show m

randomDelay :: IO ()
randomDelay = do r <- randomRIO (3, 15)
                 threadDelay (r * 100000)

forkDelay :: Int -> IO () -> IO ()
forkDelay n f = replicateM_ n $ forkIO (randomDelay >> f)

multiModify :: IO ()
multiModify = do v <- newMVar 1000
                 forkDelay 5 $ updateMoney v
                 forkDelay 5 $ readMoney v
                 _ <- getLine
                 return ()


-- Atomic transaction -- STM

-- Can deadlock occured 
unsafeModify :: IO ()
unsafeModify = do v <- newMVar 10000
                  s <- newMVar [("a", 7)]
                  forkDelay 5 $ updateMoneyAndStock "a" 1000 v s
                  forkDelay 5 $ printMoneyAndStock v s
                  _ <- getLine
                  return ()


updateMoneyAndStock :: Eq a => a
                    -> Integer
                    -> MVar Integer
                    -> MVar [(a, Integer)]
                    -> IO ()
updateMoneyAndStock product price money stock =
  do s <- takeMVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- takeMVar money
               let newS = map (\(k,v) -> if k == product
                                            then (k,v-1)
                                            else (k,v)) s
               putMVar money (m + price) >> putMVar stock newS
       else putMVar stock s

printMoneyAndStock :: Show a
                   => MVar Integer
                   -> MVar [(a, Integer)]
                   -> IO ()
printMoneyAndStock money stock = do m <- readMVar money
                                    s <- readMVar stock
                                    putStrLn $ show m ++ "\n" ++ show s



-- STM

updateMoneyAndStockStm :: Eq a => a
                       -> Integer
                       -> TVar Integer
                       -> TVar [(a, Integer)]
                       -> STM ()
updateMoneyAndStockStm product price money stock =
  do s <- readTVar stock
     let Just productNo = lookup product s
     if productNo > 0
       then do m <- readTVar money
               let newS = map (\(k,v) -> if k == product
                                            then (k,v-1)
                                            else (k,v)) s
               writeTVar money (m + price) >> writeTVar stock newS
       else writeTVar stock s

stmBaseModify :: IO ()
stmBaseModify = do v <- newTVarIO 10000
                   s <- newTVarIO [("a", 7)]
                   forkDelay 5 $ atomically $ updateMoneyAndStockStm "a" 1000 v s
                   _ <- getLine
                   return ()


pay :: Eq a => a
    -> Integer
    -> TVar Integer
    -> TVar [(a, Integer)]
    -> STM ()
pay product price money stock
  = payByCard product price money stock `orElse`
    payByCash product price money stock

                                    
payByCard :: Eq a => a
          -> Integer
          -> TVar Integer
          -> TVar [(a, Integer)]
          -> STM ()
payByCard product price money stock =
  do working <- isCardSystemWorking
     if not working
     then retry
     else updateMoneyAndStockStm product price money stock

isCardSystemWorking :: STM Bool
isCardSystemWorking = unsafeIOToSTM $ randomIO

payByCash :: Eq a => a
          -> Integer
          -> TVar Integer
          -> TVar [(a,Integer)]
          -> STM ()
payByCash product price money stock =
  updateMoneyAndStockStm product price money stock


-- Producer-Consumer Queues

queues = do q <- newTQueueIO
            forkIO $ backend q
            replicateM_ 10 $ forkIO (frontend q)
            _ <- getLine
            return ()
            

backend :: TQueue (String,Integer) -> IO ()
backend q = do
  m <- newTVarIO 10000
  s <- newTVarIO [("a",7)]
  forever $ atomically $ do (product,price) <- readTQueue q
                            pay product price m s
            
frontend :: TQueue (String,Integer) -> IO ()
frontend q = do (product,price) <- return ("a", 1)
                atomically $ writeTQueue q (product,price)
