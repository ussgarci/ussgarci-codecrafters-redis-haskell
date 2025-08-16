{-# LANGUAGE OverloadedStrings #-}

module Parser.RedisRequest (RedisRequest (..), parseRequest, parseArrayPrefix, parseBulkString)
where

import Control.Monad (fail, replicateM)
import Data.Attoparsec.ByteString.Char8 (isDigit_w8)
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

data RedisRequest = RedisRequest
    { _name :: BC.ByteString
    , _args :: [BC.ByteString]
    }
    deriving (Show)

type Parser = Parsec Void BC.ByteString

parseCommand :: Parser RedisRequest
parseCommand = do
    str <- takeWhile1P (Just "redis command") (/= (charCodesMap M.! '\r'))
    _ <- MPB.crlf
    return $ RedisRequest str []

parseArrayPrefix :: Parser Integer
parseArrayPrefix = do
    MPB.char (charCodesMap M.! '*')
    count <- takeWhile1P (Just "array prefix") isDigit_w8
    _ <- MPB.crlf
    case BC.readInteger count of
        Just (i, _) -> return i
        _ -> fail "failed to parse array prefix"

parseBulkString :: Parser Integer
parseBulkString = do
    MPB.char (charCodesMap M.! '$')
    count <- takeWhile1P (Just "bulk string") isDigit_w8
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
        return (RedisRequest str [])

parseRequest :: Parser [RedisRequest]
parseRequest =
    try parseArrayCommand
        <|> many (try parseCommand)
