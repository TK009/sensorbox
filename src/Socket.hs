module Socket where

import Control.Monad.IO.Class (liftIO)

import Network.Simple.TCP (Socket, SockAddr)
import Network.Socket (socketToHandle)
import Control.Exception (try, finally, displayException, ErrorCall)
import Control.Concurrent (forkIO)
import System.IO (Handle, hSetBuffering, BufferMode (..), hClose, IOMode (..), hGetChar)
import Data.Text.IO (hPutStrLn)
import TextShow

import Protocol
import ProtocolImpl
import Shared


handleConn :: Shared -> (Socket, SockAddr) -> IO ()
handleConn s (connectionSocket, remoteAddr) = do
    threadId <- forkIO $ do
        handle <- socketToHandle connectionSocket ReadWriteMode
        receiveRequest s handle `finally` hClose handle
    putStrLn $ "[INFO] Forked (thread:" ++ show threadId ++ ") Connection from " ++ show remoteAddr
    return ()

receiveRequest :: Shared -> Handle -> IO ()
receiveRequest s handle = do
    hSetBuffering handle LineBuffering
    loop
  where
    loop = do
        line <- hGetCmd handle "" -- hGetLine handle
        case parseRequest line of
            Just cmd -> do
                result <- processRequest s cmd
                sendResponse handle result
            Nothing ->
                putStrLn $ "[WARN] parsing failed: " ++ line
        loop

    -- | Parser for supporting different endings for requests
    hGetCmd :: Handle -> String -> IO String
    hGetCmd h partial = do
        chr <- hGetChar h

        case chr of
            ';'  -> return partial
            '\n' -> return partial
            '\r' -> hGetCmd handle partial
            _    -> hGetCmd handle (partial ++ [chr])

sendResponse :: Handle -> Response -> IO ()
sendResponse handle (Raw responseText) = hPutStrLn handle responseText
sendResponse handle response           = hPutStrLn handle $ showt response

parseRequest :: String -> Maybe Request
parseRequest req = undefined



