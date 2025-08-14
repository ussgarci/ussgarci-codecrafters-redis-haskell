{-# LANGUAGE OverloadedStrings #-}

module Parser.RedisRequest (RedisRequest (..), parseRequest)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord, isNumber)
import qualified Data.Map as M
import Data.Void (Void)
import Data.Word (Word8)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Byte as MPB
import GHC.Integer (eqInteger)

charCodesMap =
    M.fromList
        [ (' ', fromIntegral (ord ' ') :: Word8)
        , (':', fromIntegral (ord ':') :: Word8)
        , ('\r', fromIntegral (ord '\r') :: Word8)
        , ('\n', fromIntegral (ord '\n') :: Word8)
        , ('*', fromIntegral (ord '*') :: Word8)
        , ('$', fromIntegral (ord '$') :: Word8)
        ]

w8Numbers = map (\x -> fromIntegral (ord x) :: Word8) ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

newtype RedisRequest = RedisRequest
    { _command :: BC.ByteString
    }
    deriving (Show)

type Parser = MP.Parsec Void BC.ByteString

parseCommand :: Parser RedisRequest
parseCommand = do
    cmd <- MP.choice [MPB.string "PING"]
    _ <- MPB.crlf
    return $ RedisRequest cmd

parseArrayPrefix :: Parser Integer
parseArrayPrefix = do
    MPB.char (charCodesMap M.! '*')
    count <- MP.takeWhile1P (Just "array prefix") (`elem` w8Numbers)
    _ <- MPB.crlf
    case BC.readInteger count of
        Just (i, _) -> return i
        _ -> undefined 

parseBulkString :: Parser Integer
parseBulkString = do
    MPB.char (charCodesMap M.! '$')
    count <- MP.takeWhile1P (Just "bulk string") (`elem` w8Numbers)
    _ <- MPB.crlf
    case BC.readInteger count of
        Just (i, _) -> return i
        _ -> undefined 

parseRequest :: Parser [RedisRequest]
parseRequest = do
    -- try to parse the array prefix if it's there
    -- if it is, call parse bulk string n times
    -- then parse commands n times
    -- otherwise parse inline command
    MP.many $ MP.try parseCommand
