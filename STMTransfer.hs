module STMTransfer (
    main
) where

import Data.TCache
import Control.Concurrent     (forkIO, forkOS, ThreadId, threadDelay)
import Control.Concurrent.STM
import Control.Monad          (mapM, void)
import System.Random          (getStdRandom, randomR)

type Account = TMVar Float

ms :: Int
ms = 1000

openAccount :: Float -> STM (Account)
openAccount balance = newTMVar balance

transfer :: Account -> Account -> Float -> STM ()
transfer accountA accountB amount = 
    orElse (
        do
            --safeIOToSTM $ getStdRandom (randomR (0, 10*ms)) >>= threadDelay
            startingBalanceA <- takeTMVar accountA
            startingBalanceB <- takeTMVar accountB
            
            let finalBalanceA = (startingBalanceA - amount)
            let finalBalanceB = (startingBalanceB + amount)
            
            Control.Concurrent.STM.check (finalBalanceA >= 0)
            Control.Concurrent.STM.check (finalBalanceB >= 0)
            
            putTMVar accountA finalBalanceA
            putTMVar accountB finalBalanceB
            
            printOut accountA accountB
            
            safeIOToSTM $ putStrLn $ 
                ("OK: " ++ (show amount) ++
                 " A: "  ++ (show startingBalanceA) ++ "->" ++  (show finalBalanceA) ++
                 " B: "  ++ (show startingBalanceB) ++ "->" ++  (show finalBalanceB))
    ) (
            safeIOToSTM $ putStrLn $ "Transaction cancelled"
    )

printOut :: Account -> Account -> STM ()
printOut accountA accountB = do
    balanceA <- readTMVar accountA
    balanceB <- readTMVar accountB
    safeIOToSTM $ print (balanceA, balanceB)

main :: IO ()
main = do
    accountA <- atomically (openAccount 20)
    accountB <- atomically (openAccount 50)
    
    
    let actions = (take 3 . concat. repeat) [
                  forkIO $ atomically (transfer accountA accountB 30)
                  , forkIO $ atomically (transfer accountA accountB (-7))
                  , forkIO $ atomically (transfer accountA accountB 5)
                  , forkIO $ atomically (transfer accountA accountB (-5))
                  , forkIO $ atomically (transfer accountA accountB 7)
                  , forkIO $ atomically (transfer accountA accountB (-30))]
    void $ sequence actions
