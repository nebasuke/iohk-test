module NodeListParser where

import Text.Parsec
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

whiteSpace :: Parser()
whiteSpace = P.whiteSpace lexer








