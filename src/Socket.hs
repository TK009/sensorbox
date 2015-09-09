module Socket where

import Data.Maybe (listToMaybe)
import Network.Simple.TCP (Socket, SockAddr)
import Network.Socket (socketToHandle)
import Control.Exception (catch, displayException, IOException) -- try, displayException, ErrorCall)
import System.IO (Handle, hSetBuffering, BufferMode (..), hClose, IOMode (..), hGetChar)

import Protocol
import ProtocolImpl
import Shared


handleConn :: Shared -> (Socket, SockAddr) -> IO ()
handleConn s (connectionSocket, remoteAddr) = do
    handle <- socketToHandle connectionSocket ReadWriteMode
    hSetBuffering handle LineBuffering
    putStrLn $ "[INFO] Forked: Connection from " ++ show remoteAddr
    catch (receiveRequests s handle) displayError
    hClose handle -- Closing probably not needed
    putStrLn $ "[INFO] Closed: Connection from" ++ show remoteAddr
    return ()
  where
      displayError e =
          putStrLn $ "[DEBUG] " ++ show remoteAddr ++ " " ++ displayException (e :: IOException)

receiveRequests :: Shared -> Handle -> IO ()
receiveRequests s handle = loop
  where
    parseFail line = putStrLn $ "[WARN] parsing failed: " ++ line
    loop = receiveRequest s handle parseFail >> loop

-- | Receive, parse and process a request from Handle,
-- run the given function for any invalid requests.
receiveRequest :: Shared -> Handle -> (String -> IO ()) -> IO ()
receiveRequest s handle parseFailure = do
    line <- hGetCmd handle "" -- hGetLine handle
    case parseRequest line of
        Just cmd -> do
            result <- processRequest s cmd
            sendResponse handle result
        Nothing ->
            parseFailure line

  where

    -- | Parser for supporting different endings for requests
    hGetCmd :: Handle -> String -> IO String
    hGetCmd h partial = do
        chr <- hGetChar h

        case chr of
            ';'  -> return partial
            '\n' -> return partial
            '\r' -> hGetCmd handle partial
            c    -> hGetCmd handle (partial ++ [c])


parseRequest :: String -> Maybe Request
parseRequest = fmap fst . listToMaybe . reads



