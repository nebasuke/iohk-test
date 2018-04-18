{-# LANGUAGE OverloadedStrings #-}

module NodeListParser(
  endPoints
  ) where

import Data.String (fromString)
import Text.Hostname (validHostname)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

type Host = String
type Port = Integer

data EndPoint = EndPoint Host Port

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

endPoints :: Parser [EndPoint]
endPoints = do
  whiteSpace
  many1 (lexeme endPoint)

endPoint :: Parser EndPoint
endPoint = do
  hostname <- anyChar `manyTill` colon
  port <- decimal
  if validHostname (fromString hostname) || port > 65535
    then return $ EndPoint hostname port
    else fail $ "Invalid host name or port : " ++ show hostname
             ++ " port: " ++ show port

