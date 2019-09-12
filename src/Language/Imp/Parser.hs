{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Imp.Parser where

import           Text.Megaparsec.Char
import           Text.Megaparsec
import qualified Data.Text as T
import           Language.Imp.Syntax
import           Data.Void
import           Data.Char

type ImpParser = Parsec Void T.Text

parser :: T.Text -> Either T.Text CompUnit
parser = either (Left . T.pack . show) Right . parse functions ""

functions :: ImpParser CompUnit
functions = 
  CompUnit <$> function `sepBy` space

function :: ImpParser Function
function = do
  string "fun"
  space
  n <- ident
  params <- brackets (paramParser `sepBy` comma)
  body <- block
  pure (Function n params body)

  where paramParser = do
          string "var"
          space
          ident
  
ident :: ImpParser Ident 
ident = fmap (Ident . T.pack) parseIdent
  where parseIdent = do
          b <- lowerChar
          r <- many alphaNumChar
          space
          pure (b : r)

literal :: ImpParser Literal
literal =
  (Integer . read)  <$> intParser  <|>
  Character         <$> charParser <|>
  (String . T.pack) <$> textParser <*
  space

  where intParser  = some digitChar
        charParser = quote *> printChar <* quote
        textParser = doublequote *> many printChar' <* doublequote
        printChar' = satisfy (\e -> e /= '\"' && isPrint e)
                     <?> "printable character"

expr :: ImpParser Expr
expr =
  Lit <$> literal <|>
  varOrFun

  where funCall =    
          brackets (expr `sepBy` comma)
        varOrFun = do
          name <- ident
          try (FunctionCall name <$> funCall) <|> Var <$> pure name
          
statement :: ImpParser Statement
statement = 
  ifParser               <|>
  loopParser             <|>
  declParser             <|>
  returnParser           <|>
  assignmentOrExprParser 
  

  where ifParser = do
          string "if"
          space
          e <- brackets expr
          suc <- block
          space
          string "else"
          space
          fal <- block
          space
          pure (If e suc fal)

        returnParser = do
          string "return"
          space
          e <- expr
          space
          semicolon
          pure (Return e)
          
        loopParser = do
          string "loop"
          space
          e <- brackets expr
          body <- block
          space
          pure (Loop e body)

        declParser = do
          string "var"
          space
          n <- ident
          string "="
          space
          val <- try (Just <$> expr) <|> pure Nothing
          space
          semicolon
          space
          pure (Declaration (Decl n val))

        assignmentOrExprParser = do
          v <- try (Left <$> assignEq) <|> Right <$> expr
          case v of
            Left n -> do
              rhs <- expr
              space
              semicolon
              space
              pure (Assignment (Assn n rhs))
            Right e -> do
              space
              semicolon
              space
              pure (StatementExpr e)

        assignEq = do
          i <- ident
          string "="
          space
          pure i
          
block :: ImpParser Block
block = Block <$> braces (many (statement <* space))

quote :: ImpParser Char
quote = char '\''

doublequote :: ImpParser Char
doublequote = char '\"'

braces :: ImpParser p -> ImpParser p
braces p = between (char '{') (char '}') (space *> p <* space)

brackets :: ImpParser p -> ImpParser p
brackets p = between (char '(') (char ')') (space *> p <* space) <* space

semicolon :: ImpParser Char
semicolon = char ';'

comma :: ImpParser Char
comma  = char ',' <* space
