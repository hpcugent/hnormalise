module Main where

import System.Posix.Signals (installHandler, Handler(Catch), Handler(CatchOnce), sigINT, sigTERM)
import Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar, tryTakeMVar, putMVar, MVar, newEmptyMVar)

import System.ZMQ4
import Debug.Trace

handler :: MVar () -> Handler
handler s_interrupted = CatchOnce $ trace "modifying" $ putMVar s_interrupted ()

main :: IO ()
main = do
    withContext $ \ctx -> do
        withSocket ctx Pull $ \socket -> do
            bind socket "tcp://*:5555"
            s_interrupted <- newEmptyMVar
            installHandler sigINT (handler s_interrupted) Nothing
            installHandler sigTERM (handler s_interrupted) Nothing
            recvFunction s_interrupted socket
        trace "outside withSocket" $ return ()
    trace "outside withContext" $ return ()


recvFunction :: (Receiver b) => MVar () -> Socket b -> IO ()
recvFunction mi sock = do
    msg <- receive sock
    putStrLn $ "message received: " ++ show msg
    val <- tryTakeMVar mi
    case val of
        Just _  -> putStrLn "W: Interrupt received. Killing Server"
        Nothing -> recvFunction mi sock
{-}    withMVar mi (\val -> if val > 0
        then  putStrLn "W: Interrupt Received. Killing Server"
        else  recvFunction mi sock)
-}
{-

import System.Posix.Signals
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar

termHandler :: MVar () -> Handler
termHandler v = CatchOnce $ do
    putStrLn "Caught SIGTERM"
    putMVar v ()

loop :: MVar () -> IO ()
loop v = do
    putStrLn "Still running"
    threadDelay 1000000
    val <- tryTakeMVar v
    case val of
        Just _ -> putStrLn "Quitting" >> return ()
        Nothing -> loop v

main = do
    v <- newEmptyMVar
    installHandler sigTERM (termHandler v) Nothing
    loop v⏎

-}
