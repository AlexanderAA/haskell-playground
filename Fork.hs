module Fork (
    main
) where

import Control.Concurrent (forkIO, forkOS, ThreadId, threadDelay)
import Control.Monad      (mapM, void)
import System.Random      (getStdRandom, randomR)


second :: Int
second = 1000000 --microseconds


action :: String -> IO ()
action message = do
    delay <- getStdRandom (randomR (0, 5*second))
    threadDelay delay
    putStrLn $ (show delay) ++ " * " ++ message

main :: IO ()
main = do
    let messages = map (("M" ++) . show) [1..]
    let actions  = map (forkOS . action) messages 
    let workUnit = take 10 actions
    result <- sequence workUnit
    print result
    
    --threadDelay (10*second)
