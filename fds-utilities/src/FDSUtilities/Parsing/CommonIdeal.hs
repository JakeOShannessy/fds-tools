{-# LANGUAGE FlexibleContexts      #-}
module FDSUtilities.Parsing.CommonIdeal where

import FDSUtilities.Types.Common (Version(..))
import Text.Parsec

parseVersion :: (Monad m, Stream s m Char) => ParsecT s u m Version
parseVersion = Version
    <$> intNum <* char '.' <*> intNum <* char '.' <*> intNum
        <?> "Version"

floatNum :: (Monad m, Stream s m Char) => ParsecT s u m Double
floatNum = signage <*> (read <$> many1 (oneOf "0123456789-+Ee."))

intNum :: (Monad m, Stream s m Char) => ParsecT s u m Int
intNum = signage <*> nat <?> "Int"

signage :: (Monad m, Stream s m Char, Num a) => ParsecT s u m (a -> a)
signage = (const negate <$> char '-') <|> (const id <$> char '+') <|> pure id

nat :: (Monad m, Stream s m Char) => ParsecT s u m Int
nat = read <$> many1 digit

eol :: (Monad m, Stream s m Char) => ParsecT s u m ()
eol = optional (char '\r') *> char '\n' *> pure () <?> "end of line"

onlySpaces :: (Monad m, Stream s m Char) => ParsecT s u m String
onlySpaces = many (oneOf " \t")

eofString :: (Monad m) => ParsecT String u m String
eofString = do
    eof
    return "eof"