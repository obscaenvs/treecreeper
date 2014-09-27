{-# LANGUAGE OverloadedStrings #-}
module Treecreeper where

import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Char 
import Text.Parsec.String
import Data.Text (Text)
import Data.Tree hiding (subForest)
import Data.Time (UTCTime)
import Data.Monoid (mconcat)
import System.Environment (getArgs)
import Control.Exception (catch, SomeException) 
import Control.Monad.Identity (Identity, liftM2)
import Control.Applicative ((*>), (<*), (<$>), (<*>))

--------------------------------------------------------------------------------
--                                                                            --
--                             Function definitions                           --
--                                                                            --
--------------------------------------------------------------------------------

forest :: Parser (Forest String)
forest = node `initBy1` nodeToken

node :: Parser (Tree String)
node = Node <$> value <*> (try (descendToken *> subForest <* ascendToken) <|> return [])
            <?> "empty node encountered."

value :: Parser String
value = try (anyChar `many1Till` (noConsume stopTokens)) <|>
        (anyChar `many1Till` eof)
    where stopTokens = choice [nodeToken, descendToken, ascendToken]

-- allows an empty subforest, but's that intended.
subForest :: Parser (Forest String)
subForest =  (nodeToken *> node) `manyTill` (noConsume ascendToken)

mkToken :: String -> Parser String
mkToken str = try $ withSpaces (string str <* notFollowedBy alphaNum)

nodeToken :: Parser String
nodeToken = mkToken ":node"

descendToken :: Parser String
descendToken = mkToken ":descend"

ascendToken :: Parser String
ascendToken = mkToken ":ascend"

withSpaces :: Parser a -> Parser a
withSpaces p = spaces *> p <* spaces

--------------------------------------------------------------------------------
--                                                                            --
--    Function definitions that should be in the official Parsec API...       --
--                                                                            --
--------------------------------------------------------------------------------

initBy1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
initBy1 p sep = many1 (sep >> p)

many1Till :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
many1Till p end = scan
    where scan = do 
                   x <- p
                   do { end *> return [x] } <|> do { xs <- scan; return (x:xs) }

noConsume :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a
noConsume = try . lookAhead

--------------------------------------------------------------------------------
--                                                                            --
--                                     Main                                   --
--                                                                            --
--------------------------------------------------------------------------------

main = do
  args <- getArgs
  let fileName =
          case args of
            (a:as) -> a
            _ -> "testinput.txt"
  input <- catch (readFile fileName) $
                 \err -> do
                   print (err :: SomeException)
                   return ""
  putStr input



