{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Redundant bracket" #-}

module Main (main) where

import Control.Monad (forever, replicateM_, void)
import Control.Monad.State
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

-- import Data.List.Split (splitOn)
import Data.Char (ord)
import qualified Data.Map as M
import Data.Word (Word8)
import Network.Simple.TCP (HostPreference (HostAny), Socket, closeSock, recv, send, serve)
import Parser.RedisRequest
import System.IO (BufferMode (NoBuffering), hPutStrLn, hSetBuffering, stderr, stdout)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Byte as MPB

processCommand :: Socket -> RedisRequest -> StateT AppState IO ()
processCommand socket (RedisRequest{_name = "PING"}) = send socket $ C8.pack "+PONG\r\n"
processCommand _ _ = undefined

newtype AppState = AppState
    { _buffer :: B.ByteString
    }

scan :: Socket -> StateT AppState IO ()
scan socket = do
    buffer <- recv socket 4096
    case buffer of
        Just request -> do
            -- prev <- _buffer <$> get
            liftIO $ print ("request: " <> request)
            prev <- gets _buffer
            liftIO $ print ("prev: " <> prev)
            let combined = prev <> request
            liftIO $ print ("combined: " <> combined)
            case MP.runParser ((,) <$> parseRequest <*> MP.getInput) "" combined of
                Left err -> do
                    liftIO $ print "error"
                    modify $ \s -> s{_buffer = combined}
                    scan socket
                Right (parsed, leftover) -> do
                    liftIO $ print ("parsed: " <> show parsed)
                    liftIO $ print ("leftover: " <> show leftover)
                    modify $ \s -> s{_buffer = leftover}
                    mapM_ (processCommand socket) parsed
                    scan socket
        Nothing -> closeSock socket

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port
    serve HostAny port $ \(socket, address) -> do
        putStrLn $ "successfully connected client: " ++ show address
        void $ execStateT (scan socket) (AppState B.empty)
