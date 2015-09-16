module Socket where

import Data.Maybe (listToMaybe)
import Network.Simple.TCP (Socket, SockAddr)
import Network.Socket (socketToHandle, HostName, ServiceName, getAddrInfo
    , defaultHints, socket, AddrInfo (..), SocketType (..), AddrInfoFlag (..), connect)
import Control.Exception (catch, displayException, IOException, try, finally, throw)
import System.IO (Handle, hSetBuffering, BufferMode (..), hClose, IOMode (..), hGetChar)
import Control.Concurrent (forkIO, ThreadId)

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
    putStrLn $ "[INFO] Closed: Connection from " ++ show remoteAddr
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

-- | Simple connection function that tries all combinations
forkConnect :: HostName -> ServiceName -> (Handle -> IO ()) -> IO ()
forkConnect host port act = do
    hdl <- connectSocket host port
    tid <- forkIO $
        act hdl `finally` hClose hdl
    putStrLn $ "[DEBUG] Forked connection thread " ++ show tid

connectSocket :: HostName -> ServiceName -> IO Handle
connectSocket host port = do
    infos <- getAddrInfo (Just hints) (Just host) (Just port)
    tryConnect infos

  where
    hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                         , addrSocketType = Stream
                         }
    newSocket addr = socket (addrFamily addr)
                            (addrSocketType addr)
                            (addrProtocol addr)

    -- tries all addresses until some connects, throws error if nothing connects
    tryConnect [] = error "suppresses warning"
    tryConnect (addr : addrs) = do
        res <- try $ do
            sock <- newSocket addr
            let testAddr = addrAddress addr
            connect sock testAddr
            socketToHandle sock ReadWriteMode
        case res of
            Left e -> if null addrs
                then throw (e :: IOException)
                else tryConnect addrs
            Right hdl -> return hdl






