module Main where

import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar, MVar)

import System.ZMQ4
import Debug.Trace

handler :: MVar Int -> IO ()
handler s_interrupted = trace "modifying ..." $ modifyMVar_ s_interrupted (return . (+1))

main :: IO ()
main = do
    withContext $ \ctx -> do
        withSocket ctx Pull $ \socket -> do
            bind socket "tcp://*:5555"
            s_interrupted <- newMVar 0
            installHandler sigINT (Catch $ handler s_interrupted) Nothing
            installHandler sigTERM (Catch $ handler s_interrupted) Nothing
            recvFunction s_interrupted socket
        trace "outside withSocket" $ return ()
    trace "outside withContext" $ return ()


recvFunction :: (Ord a, Num a, Receiver b) => MVar a -> Socket b -> IO ()
recvFunction mi sock = do
    msg <- receive sock
    putStrLn $ "message received: " ++ show msg
    withMVar mi (\val -> if val > 0
        then putStrLn "W: Interrupt Received. Killing Server"
        else recvFunction mi sock)
