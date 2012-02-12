module Main where

import Control.Applicative ((<$>), (<*>), (<|>), many)
import Control.Monad (forM_, guard, void, when)
import Data.List (delete)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (IOMode(..), hPutStrLn, hPrint, stderr, withFile)
import Text.Parsec (getPosition, try, parse)
import Text.Parsec.Char (upper, lower, alphaNum, oneOf)
import Text.Parsec.Combinator (choice, optional , sepEndBy, notFollowedBy, manyTill)
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Pos (SourcePos, sourceName, sourceLine)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P

main :: IO ()
main = do
    (_orig:inp:out:_) <- getArgs
    src <- readFile inp
    case parse getFirstImportPosition inp src of
        Left err  -> hPrint stderr err >> exitFailure
        Right pos -> importModule "Development.Placeholders" pos out

getFirstImportPosition :: Parser SourcePos
getFirstImportPosition = optional module' >> getPosition

importModule :: String -> SourcePos -> FilePath -> IO ()
importModule mod pos out = withFile out AppendMode $ \h -> do
    con <- readFile (sourceName pos)
    forM_ (zip [1..] (lines con)) $ \(lineNo, line) -> do
        when (lineNo == sourceLine pos) $
             hPutStrLn h $ "import " ++ mod
        hPutStrLn h line

----------------------------------------------------------
-- Module header parser from Haskell 2010 Report

module' :: Parser ()
module' = do
    whiteSpace
    reserved "module"
    void modid
    optional exports
    reserved "where"

exports :: Parser ()
exports = void . parens $ export' `sepEndBy` comma

export' :: Parser ()
export' = choice
    [ try $ reserved "module" >> modid
    , try $ modid >> optional explicitList
    , qvar
    ]
    where
        explicitList = parens $ choice
            [ void $ dot >> dot
            , void $ commaSep cname <|> commaSep var
            ]

cname :: Parser ()
cname = var <|> con

var :: Parser ()
var = varid <|> parens varsym

con :: Parser ()
con = conid <|> parens consym

varsym :: Parser ()
varsym = lexeme $ do
    xs <- (:) <$> oneOf (delete ':' symbol')
              <*> many symbol
    guard $ all (xs /=) reservedop

consym :: Parser ()
consym = lexeme $ do
    xs <- (++) <$> colon
               <*> many symbol
    guard $ all (xs /=) reservedop

qvar :: Parser ()
qvar = try qvarid <|> parens qvarsym

qvarid :: Parser ()
qvarid = qualified varid

qvarsym :: Parser ()
qvarsym = qualified varsym

varid :: Parser ()
varid = void . lexeme $ lower >> many (alphaNum <|> oneOf "_\'")

conid :: Parser ()
conid = void . lexeme $ upper >> many (alphaNum <|> oneOf "_\'")

modid :: Parser ()
modid = qualified conid

qualified :: Parser a -> Parser ()
qualified p = void . lexeme $ manyTill (conid >> dot)
                                       (try $ p >> notFollowedBy dot)

symbol :: Parser Char
symbol = oneOf symbol'

symbol' :: [Char]
symbol' = "!#$%&*+./<=>?@\\^|-~:"

reservedop :: [String]
reservedop = ["..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

----------------------------------------------------------
-- Lexers

lexer = P.makeTokenParser haskellDef

parens :: Parser a -> Parser a
parens = P.parens lexer

reserved :: String -> Parser ()
reserved = P.reserved lexer

commaSep :: Parser a -> Parser [a]
commaSep = P.commaSep lexer

dot :: Parser String
dot = P.dot lexer

lexeme :: Parser a -> Parser a
lexeme = P.lexeme lexer

colon :: Parser String
colon = P.colon lexer

comma :: Parser String
comma = P.comma lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer
