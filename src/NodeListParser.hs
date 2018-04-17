{-# LANGUAGE OverloadedStrings #-}

module NodeListParser where

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

endPoints :: Parser [EndPoint]
endPoints = undefined

endPoint :: Parser (Either String EndPoint)
endPoint = do
  hostname <- anyChar `manyTill` colon
  port <- decimal
  return $
    if validHostname (fromString hostname)
      then Right $ EndPoint hostname port
    else Left $ "Invalid host name: " ++ (show hostname)


