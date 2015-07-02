module DnoList.SMTPCommands
       ( Sysop(..)
       , sysop
       ) where

import Control.Applicative
import Data.Text (Text)
import Data.Char
import Data.Attoparsec.Text

data Sysop = CreateList Text Text
           | DeleteList Text
           | Subscribe Text
           | Unsubscribe Text
           deriving (Show, Eq)

sysop :: Parser Sysop
sysop = skipSpace *> (createList <|> deleteList <|> subscribe <|> unsubscribe) <* skipSpace

createList :: Parser Sysop
createList = do
  string "create-list"
  skipSpace
  name <- takeWhile1 $ not . isSpace
  skipSpace
  title <- takeWhile1 $ not . isSpace
  return $ CreateList name title

deleteList :: Parser Sysop
deleteList = do
  string "delete-list"
  skipSpace
  name <- takeWhile1 $ not . isSpace
  return $ DeleteList name

subscribe :: Parser Sysop
subscribe = do
  string "subscribe"
  skipSpace
  name <- takeWhile1 $ not . isSpace
  return $ Subscribe name

unsubscribe :: Parser Sysop
unsubscribe = do
  string "unsubscribe"
  skipSpace
  name <- takeWhile1 $ not . isSpace
  return $ Unsubscribe name
