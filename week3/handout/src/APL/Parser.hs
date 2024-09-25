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

lBool :: Parser Bool
lBool = 
  try $ lexeme $ choice
            [const True <$> lKeyword "true",
            const False <$> lKeyword "false"]

lKeyword :: String -> Parser()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

--check whether is such a string
lString :: String -> Parser()
lString s = lexeme $ void $ chunk s


--(<$>) :: Functor f => (a -> b) -> f a -> f b

--屁股一侧只执行不取用结果
--(*>)  :: Applicative f => f a -> f b -> f b
--(<*)  :: Applicative f => f a -> f b -> f a

pAtom :: Parser Exp
pAtom = choice [
          CstInt <$> lInteger,
          CstBool <$> lBool,
          Var <$> lVName,
          lString "(" *> pExp <* lString ")"
        ]


--(<$>) :: Functor f => (a -> b) -> f a -> f b
--屁股一侧只执行不取用结果
--(*>)  :: Applicative f => f a -> f b -> f b

--(<*>)  :: Applicative f => f(a->b) ->  f a -> f b 

pLExp :: Parser Exp
pLExp = choice 
          [ If <$> (lKeyword "if" *> pExp0) ---If is constructor from Exp type
              <*> (lKeyword "then" *> pExp0)
              <*> (lKeyword "else" *> pExp0), ----- 整个是个parser
           pAtom ---另一个parser， 若不是if-then-not expression , falls back to pAtom
          ]
-- + - lowerest precedence, then *\, atom higheet? 

pExp1 :: Parser Exp
pExp1 = pLExp >>= chain
  where chain x =
          choice 
          [ do 
              lString "*" --不用，但还是被消耗了
              y <- pLExp
              chain $ Mul x y, 
              ---x is current left-hand side, atomic
              -- y is newly parsed atomic expression
            do
              lString "/"
              y <- pLExp
              chain $ Div x y,
            --Base Case (pure x): When no more operators are encountered, 
            --the parser returns the current expression x, 
            --ending the recursion.  
            pure x
          ]

-- (>>=):: m a -> (a -> m b) -> m b
pExp0 :: Parser Exp
pExp0 = pExp1 >>= chain
  where chain x =
          choice 
          [ do
              lString "+"
              y <- pExp1          ---x , y are exp
              chain $ Add x y,    ---Add x y , is also a expression
            do 
              lString "-"
              y <- pExp1
              chain $ Sub x y,  -- y is newly parsed atomic exp
                                --x is current left-hand side
                                --left hand is always atomic?
            pure x
          ]

pExp :: Parser Exp
pExp = pExp0

--先执行最低级， 
--eExp:: top-level
--pExp0( +- ) 
--then pExp1(*/) 
--then pLExp (if, if pEXP0.. then pAtom. in pAtom, 可以有pExp)
--pAtom highest, evnn "exp", since no OPERATION


--pAtom like highest? so先atom ? 再pexp1?
--only the lowest has non-atomic exp in the left?