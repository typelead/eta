
module Java.META.Parser
  (parseMeta,
   parseMetaFile) where

import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String

import Java.META.Types

pNewLine :: Parser ()
pNewLine = choice $ map try $ [
              string "\r\n" >> return (),
              char '\r' >> return (),
              char '\n' >> return () ]

blankline :: Parser ()
blankline = do
  pNewLine <?> "first newline in blankline"
  return ()

pSection :: Parser Section
pSection = do
  list <- many1 pHeader
  blankline <?> "blank line"
  return $ M.fromList list

pHeader :: Parser (String, String) 
pHeader = do
    name <- many1 headerChar <?> "header name"
    char ':'
    value <- pValue
    return (name, value)
  where
    headerChar = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

pValue :: Parser String
pValue = do
  list <- manyLines
  return (concat list)

manyLines :: Parser [String]
manyLines = do
  char ' '
  many space
  s <- many1 (noneOf "\n\r\0")
  pNewLine <?> "new line at end of value line"
  c <- lookAhead anyChar
  if c == ' '
    then do
         next <- manyLines
         return (s: next)
    else return [s]

pMETA :: Parser META
pMETA = many1 pSection

parseMetaFile :: FilePath -> IO (Either ParseError META)
parseMetaFile path = do
  str <- readFile path
  return $ parse pMETA path (str ++ "\n\n")

parseMeta :: String -> Either ParseError META
parseMeta str = parse pMETA "<META>" (str ++ "\n\n")

