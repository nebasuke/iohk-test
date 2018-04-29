{-# LANGUAGE OverloadedStrings #-}
module NodeListParser(
  endPoint, endPoints,
  parseEndPoints
  ) where

import Data.String (fromString)
import Text.Hostname (validHostname)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

import Types

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

colon :: Parser String
colon = P.colon lexer

decimal :: Parser Integer
decimal = P.decimal lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

-- | Given an input String, run the endPoints parser on it.
parseEndPoints :: String -> Either ParseError [EndPoint]
parseEndPoints input = parse endPoints "" input

-- | A list of end points, possibly starting off with white space and having
-- white space in between end points.
endPoints :: Parser [EndPoint]
endPoints = do
  whiteSpace
  many1 (lexeme endPoint)

-- | An end point is an IP address/host:port string, satisying
-- standard host name rules and port ranges.
endPoint :: Parser EndPoint
endPoint = do
  hostname <- anyChar `manyTill` colon
  port <- decimal
  if validHostname (fromString hostname) && port <= 65535
    then return $ EndPoint hostname port
    else fail $ "Invalid host name or port : " ++ show hostname
             ++ " port: " ++ show port
