module FDSUtilities.Parsing.Indent (parseIndent) where

import Text.Parsec hiding (many, optional, (<|>))
-- import Text.ParserCombinators.Parsec hiding (parseFromFile)
import Text.Parsec.Indent
import Data.Char (isSpace)
import Control.Applicative
import Data.Monoid
import Data.Either.Utils (forceEither)
import Data.Tree

parseIndent :: String -> Tree String
parseIndent input = conLines $ forceEither $ runIndent $ runParserT aTree () "" input

aTree = Node "/" <$> many aNode

aNode = spaces *> withBlock makeNode aNodeHeader aNode

aNodeHeader = aLeaf <* spaces

-- aNodeHeader = aLeaf <* spaces

conLines (Node l subNodes)
    | all endNode subNodes =
        let newSub = Node (unlines $ filter (not . null) $ map rootLabel subNodes) []
        in case newSub of
            Node "" [] -> Node l []
            _ -> Node l [newSub]
    | otherwise = Node l (map conLines subNodes)


endNode (Node _ []) = True
endNode _ = False

aLeaf = do
    -- s <- many1 (satisfy (not . isSpace))
    --s <- many1 $ many1 $ oneOf "1234567890-=+qwer~!@#_$%^&*()\"tyuiop[]asdfghjkl;'zxcvbnm,./QWERTYUIOP{}ASDFGHJKL:ZXCVBNM<>? �"
    s <- many1 $ many1 $ noneOf "\r\n"
    -- many (oneOf " \t")
    return $ concat s
-- Leaf <$> (many1 (satisfy (not . isSpace)) <* many (oneOf " \t"))

-- makeNode nodeVal subNodes = Tree nodeVal subNodes
makeNode nodeVal subNodes = Node nodeVal subNodes

-- data Tree = Node [Tree] | Leaf String deriving (Show)

-- serializeIndentedTree tree = drop 2 $ s (-1) tree
    -- where
        -- s i (Node children) = "\n" <> (concat $ replicate i "    ") <> (concat$ map (s (i+1)) children)
        -- s _ (Leaf text)     = text <> " "