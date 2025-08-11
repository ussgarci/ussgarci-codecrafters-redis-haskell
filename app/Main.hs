{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM_)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Data.Char (ord)
import qualified Data.Map as M
import Data.Word (Word8)
import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock, send, recv)
import System.IO (hPutStrLn, hSetBuffering, stdout, stderr, BufferMode(NoBuffering))


charCodesMap :: M.Map Char Word8
charCodesMap =
    M.fromList
        [ (' ', fromIntegral (ord ' ') :: Word8)
        , (':', fromIntegral (ord ':') :: Word8)
        , ('\r', fromIntegral (ord '\r') :: Word8)
        , ('\n', fromIntegral (ord '\n') :: Word8)
        ]

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    hPutStrLn stderr "Logs from your program will appear here"

    let port = "6379"
    putStrLn $ "Redis server listening on port " ++ port
    serve HostAny port $ \(socket, address) -> do
        putStrLn $ "successfully connected client: " ++ show address

        -- read chunk of data into a buffer
        buffer <- recv socket 4096
        case buffer of
            Just request -> do
                --let cmds = B.splitWith (\x -> x == charCodesMap M.! '\r' || x ==  charCodesMap M.! '\n') request
                let cmdCount = B.count (charCodesMap M.! '\r') request
                print cmdCount
                replicateM_ cmdCount (send socket $ BC.pack "+PONG\r\n")
            Nothing -> closeSock socket
            --send socket $ BC.pack "+PONG\r\n" 
            ----closeSock socket
            --putStrLn $ "request: " ++ show request
            --send socket $ BC.pack "+PONG\r\n" 
            ----closeSock socket
