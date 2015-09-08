module Socket where

import Data.Maybe (listToMaybe)
import Network.Simple.TCP (Socket, SockAddr)
import Network.Socket (socketToHandle)
import Control.Exception (finally) -- try, displayException, ErrorCall)
import System.IO (Handle, hSetBuffering, BufferMode (..), hClose, IOMode (..), hGetChar)

import Protocol
import ProtocolImpl
import Shared


handleConn :: Shared -> (Socket, SockAddr) -> IO ()
handleConn s (connectionSocket, remoteAddr) = do
    handle <- socketToHandle connectionSocket ReadWriteMode
    hSetBuffering handle LineBuffering
    putStrLn $ "[INFO] Forked: Connection from " ++ show remoteAddr
    receiveRequests s handle `finally` hClose handle
    return ()

receiveRequests :: Shared -> Handle -> IO ()
receiveRequests s handle = loop
  where
    loop = receiveRequest s handle >> loop

receiveRequest :: Shared -> Handle -> IO Request
receiveRequest s handle = do
    line <- hGetCmd handle "" -- hGetLine handle
    case parseRequest line of
        Just cmd -> do
            result <- processRequest s cmd
            sendResponse handle result
        Nothing ->
            putStrLn $ "[WARN] parsing failed: " ++ line

  where

    -- | Parser for supporting different endings for requests
    hGetCmd :: Handle -> String -> IO String
    hGetCmd h partial = do
        chr <- hGetChar h

        case chr of
            ';'  -> return partial
            '\n' -> return partial
            '\r' -> hGetCmd handle partial
            _    -> hGetCmd handle (partial ++ [chr])


parseRequest :: String -> Maybe Request
parseRequest = fmap fst . listToMaybe . reads



