module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    parseTest,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

-- Do not change this definition.
type Parser = Parsec Void String

-- Do not change this definition.
parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x


-- digit = "1" | "2" |"3" | "4" | "5" | "6" | "7" | "8" | "9";
-- int = digit{digit};
-- Exp :: int  "+" int;


keywords :: [String]
keywords = [
    "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch",
    "print",
    "put",
    "get"
    ]
    
-- Atom ::= var
--         | int
--         | bool
--         |"(" Exp ")" 
-- Exp ::= Atom
--         | Exp "+" Exp
--         | Exp "-" Exp
--         | Exp "*" Exp
--         | Exp "/" Exp


Atom ::= var
       | int
       | bool
       | "(" Exp ")"

Exp0' ::=            (* empty *)
        | "+" Atom Exp0'
        | "-" Atom Exp0'
        | "*" Atom Exp0'
        | "/" Atom Exp0'

Exp0 ::= Atom Exp0'

Exp  ::= Exp0



lexeme :: Parser a -> Parser a
lexeme p = p <* space


lInteger :: Parser Integer
lInteger = lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlpha)

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c:cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lKeyword :: String -> Parser()
lKeyword s = lexeme $ void $ try $ chunk s 

lBool :: Parser Bool
lBool = try $ lexeme $ choice
                          [const True <$> lKeyword "true",
                            const False <$> lKeyword "false"]


lString :: String -> Parser()
lString s = lexeme $ void $ chunk s

pExp :: Parser Exp
pExp = choice
        [
          CstInt <$> lInteger,
          Var <$> lVName,
          CstBool <$> lBool
        ]