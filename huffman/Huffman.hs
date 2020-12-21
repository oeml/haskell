module Huffman (
    huffmanEncoding,
    encode,
    decode
) where

import Data.Word (Word8)
import Data.List (sortBy, union, find)
import Data.Function (on)
import qualified Data.Map as M

data HuffmanTree = Leaf  Word8
                 | Inode HuffmanTree HuffmanTree
    deriving (Show)

frequenciesToNodes :: [(Word8, Int)] -> [(HuffmanTree, Int)]
frequenciesToNodes = map (\ (x, n) -> (Leaf x, n)) . sortBy (compare `on` snd)

buildTree :: [(HuffmanTree, Int)] -> HuffmanTree
buildTree [(node, _)] = node
buildTree (u:(v:zs)) = buildTree $ sortByFrequency $ (Inode x y, n + m):zs
    where (x, n) = u
          (y, m) = v
          sortByFrequency = sortBy (compare `on` snd)

codesFromTree :: [Bool] -> HuffmanTree -> [(Word8, [Bool])]
codesFromTree encoding (Leaf sym) = [(sym, encoding)]
codesFromTree prefix (Inode lTree rTree) = union leftCodes rightCodes
    where leftCodes  = codesFromTree (prefix++[True]) lTree
          rightCodes = codesFromTree (prefix++[False]) rTree

huffmanEncoding :: [(Word8, Int)] -> [(Word8, [Bool])]
huffmanEncoding = codesFromTree [] . buildTree . frequenciesToNodes

encode :: [Word8] -> [(Word8, [Bool])] -> [Bool]
encode text codes = concat $ map (codeMap M.!) text
    where codeMap = M.fromList codes

decode :: [Bool] -> [(Word8, [Bool])] -> [Word8]
decode source codes = decodeHelper [] source codes []

decodeHelper :: [Bool] -> [Bool] -> [(Word8, [Bool])] -> [Word8] -> [Word8]
decodeHelper _ [] _ decoded = decoded
decodeHelper currPrefix encoded codes decoded = case maybeSym of
        Just sym -> decodeHelper [] (tail encoded) codes (decoded++[fst sym])
        Nothing -> decodeHelper newPrefix (tail encoded) codes decoded
    where newPrefix = currPrefix++[head encoded]
          maybeSym = find ((== newPrefix) . snd) codes
