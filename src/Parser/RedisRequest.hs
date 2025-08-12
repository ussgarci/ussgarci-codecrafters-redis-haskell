{-# LANGUAGE OverloadedStrings #-}

module Parser.RedisRequest (RedisRequest (..), parseRequest)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import qualified Data.Map as M
import Data.Void (Void)
import Data.Word (Word8)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Byte as MPB

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

parseRequest :: Parser [RedisRequest]
parseRequest = MP.many $ MP.try parseCommand
