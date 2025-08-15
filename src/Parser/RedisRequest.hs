{-# LANGUAGE OverloadedStrings #-}

module Parser.RedisRequest (RedisRequest (..), parseRequest, parseArrayPrefix, parseBulkString)
where

import Control.Monad (fail, replicateM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (isNumber, ord)
import qualified Data.Map as M
import Data.Void (Void)
import Data.Word (Word8)
import GHC.Integer (eqInteger)
import Text.Megaparsec
import qualified Text.Megaparsec.Byte as MPB
import Prelude hiding (take)

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

type Parser = Parsec Void BC.ByteString

parseCommand :: Parser RedisRequest
parseCommand = do
    cmd <- choice [MPB.string "PING"]
    _ <- MPB.crlf
    return $ RedisRequest cmd

parseArrayPrefix :: Parser Integer
parseArrayPrefix = do
    MPB.char (charCodesMap M.! '*')
    count <- takeWhile1P (Just "array prefix") (`elem` w8Numbers)
    _ <- MPB.crlf
    case BC.readInteger count of
        Just (i, _) -> return i
        _ -> fail "failed to parse array prefix"

parseBulkString :: Parser Integer
parseBulkString = do
    MPB.char (charCodesMap M.! '$')
    count <- takeWhile1P (Just "bulk string") (`elem` w8Numbers)
    _ <- MPB.crlf
    case BC.readInteger count of
        Just (i, _) -> return i
        _ -> fail "failed to parse bulk string"

parseArrayCommand :: Parser [RedisRequest]
parseArrayCommand = do
    count <- parseArrayPrefix
    replicateM (fromInteger count) $ do
        len <- parseBulkString
        str <- takeP Nothing (fromInteger len)
        _ <- MPB.crlf
        if str == "PING"
            then return (RedisRequest str)
            else fail ("Unsupported command: " <> show str)

parseRequest :: Parser [RedisRequest]
parseRequest =
    try parseArrayCommand
        <|> many (try parseCommand)
