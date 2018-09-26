module FDSUtilities.Parsing.Common where

import FDSUtilities.Types.Common (Version(..))
import Text.ParserCombinators.Parsec

parseVersion :: Parser Version
parseVersion = Version
    <$> intNum <* char '.' <*> intNum <* char '.' <*> intNum
        <?> "Version"

floatNum :: Parser Double
floatNum = signage <*> (read <$> many1 (oneOf "0123456789-+Ee."))

intNum :: Parser Int
intNum = signage <*> nat <?> "Int"

signage :: Num a => Parser (a -> a)
signage = (const negate <$> char '-') <|> (const id <$> char '+') <|> pure id

nat :: Parser Int
nat = read <$> many1 digit

eol :: Parser ()
eol = optional (char '\r') *> char '\n' *> pure () <?> "end of line"

onlySpaces :: Parser String
onlySpaces = many (oneOf " \t")

eofString :: Parser String
eofString = do
    eof
    return "eof"