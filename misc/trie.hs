import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Trie = Trie Int (Map.Map Char Trie) deriving (Show)

trieBuild :: String -> Trie
trieBuild text = trieBuildHelper trieEmpty (words text)

trieBuildHelper :: Trie -> [String] -> Trie
trieBuildHelper trie []           = trie
trieBuildHelper trie (word:words) = trieBuildHelper (trieInsert trie [x | x <- word, not (x `elem` ".,?!-:;\"\'")]) words

trieGet :: Trie -> String -> Int
trieGet (Trie freq _) [] = freq
trieGet (Trie _ nodes) (x:xs) = case Map.lookup x nodes of Just node -> trieGet node xs
                                                           Nothing   -> 0

trieEmpty :: Trie
trieEmpty = Trie 0 Map.empty

trieInsert :: Trie -> String -> Trie
trieInsert (Trie freq nodes) []     = Trie (freq+1) nodes
trieInsert (Trie freq nodes) (x:xs) = Trie freq (Map.insert x (trieInsert node xs) nodes)
    where node = fromMaybe trieEmpty (Map.lookup x nodes)

