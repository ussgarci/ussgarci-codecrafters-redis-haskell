{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM_, forever)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as B
-- import Data.List.Split (splitOn)
import Data.Char (ord)
import qualified Data.Map as M
import Data.Word (Word8)
import Network.Simple.TCP (serve, HostPreference(HostAny), closeSock, send, recv)
import System.IO (hPutStrLn, hSetBuffering, stdout, stderr, BufferMode(NoBuffering))

validCommands = ["PING"]

charCodesMap :: M.Map Char Word8
charCodesMap =
    M.fromList
        [ (' ', fromIntegral (ord ' ') :: Word8)
        , (':', fromIntegral (ord ':') :: Word8)
        , ('\r', fromIntegral (ord '\r') :: Word8)
        , ('\n', fromIntegral (ord '\n') :: Word8)
        ]

splitOnCRLF :: B.ByteString -> [B.ByteString]
splitOnCRLF xs =
    let (cmd, rest) = B.breakSubstring "\r\n" xs
    in
        if B.null rest
        then []
        else cmd : splitOnCRLF (B.drop 2 rest)

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
        loop socket  
        where
            loop socket = do
                -- read chunk of data into a buffer
                buffer <- recv socket 4096
                case buffer of
                    Just request -> do
                        print "REQUEST:"
                        print request
                        let cmds = splitOnCRLF request
                        let cmds' = filter (`elem` validCommands) cmds
                        print (length cmds')
                        print cmds'
                        replicateM_ (length cmds') (send socket $ C8.pack "+PONG\r\n")
                        loop socket 
                    Nothing -> closeSock socket
