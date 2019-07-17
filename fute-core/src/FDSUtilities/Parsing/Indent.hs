module FDSUtilities.Parsing.Indent (parseIndent) where

import Text.Parsec hiding (many, optional, (<|>))
-- import Text.ParserCombinators.Parsec hiding (parseFromFile)
import Text.Parsec.Indent
import Data.Char (isSpace)
import Control.Applicative
import Data.Monoid
import Data.Either.Utils (forceEither)
import Data.Tree
import Data.Functor.Identity

parseIndent :: String -> Tree String
parseIndent input = conLines $ forceEither $ runIndent $ runParserT aTree () "" input

aTree :: ParsecT String u (IndentT Identity) (Tree [Char])
aTree = Node "/" <$> many aNode

aNode :: IndentParserT String u Identity (Tree String)
aNode = spaces *> withBlock makeNode aNodeHeader aNode

aNodeHeader :: ParsecT String u (IndentT Identity) String
aNodeHeader = aLeaf <* spaces

conLines :: Tree String -> Tree String
conLines (Node l subNodes)
    | all endNode subNodes =
        let newSub = Node (unlines $ filter (not . null) $ map rootLabel subNodes) []
        in case newSub of
            Node "" [] -> Node l []
            _ -> Node l [newSub]
    | otherwise = Node l (map conLines subNodes)

endNode :: Tree a -> Bool
endNode (Node _ []) = True
endNode _ = False

aLeaf :: ParsecT String u (IndentT Data.Functor.Identity.Identity) String
aLeaf = do
    s <- many1 $ many1 $ noneOf "\r\n"
    return $ concat s

makeNode :: a -> Forest a -> Tree a
makeNode nodeVal subNodes = Node nodeVal subNodes
