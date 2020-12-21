import Huffman (huffmanEncoding, encode, decode)

import System.IO
import System.Environment (getArgs)
import Data.List (group, sort, unfoldr)
import Data.Word (Word8)
import Data.Bits (shiftL, testBit)
import Data.ByteString.Lazy (ByteString, pack, unpack)
import qualified Data.ByteString.Lazy as BS

main = do
    let select "-c" = archive
        select "-x" = extract
    (mode:inf:outf:[]) <- getArgs
    inh <- openBinaryFile inf ReadMode
    outh <- openBinaryFile outf WriteMode
    (select mode) inh outh
    hClose inh
    hClose outh

archive :: Handle -> Handle -> IO ()
archive inh outh = do
    contents <- BS.hGetContents inh
    let text = unpack contents
        codes = huffmanEncoding . frequencies $ text
    BS.hPut outh $ codeTableToByteString codes
    BS.hPut outh $ packBits $ encode text codes

extract :: Handle -> Handle -> IO ()
extract inh outh = do
    contents <- BS.hGetContents inh
    let word8s = unpack contents
        (n, rest) = splitAt 1 word8s
        codeTableSize = n !! 0
        (codes, archivedData) = splitAt (fromIntegral codeTableSize) rest
        codeTable = codeTableFromBytes codes
        encodedData = concat $ map byteToBool archivedData
        decodedData = decode encodedData codeTable
    BS.hPut outh $ pack decodedData

frequencies :: [Word8] -> [(Word8, Int)]
frequencies = map (\ x -> (head x, length x)) . group . sort

boolToByte :: [Bool] -> Maybe (Word8, [Bool])
boolToByte [] = Nothing
boolToByte xs = Just (sum $ map (shiftL 1) [x | (True, x) <- zip first8 $ reverse [0..7]], rest)
    where (first8, rest) = splitAt 8 xs

byteToBool :: Word8 -> [Bool]
byteToBool b = map (testBit b) $ reverse [0..7]

packBits :: [Bool] -> ByteString
packBits = pack . unfoldr boolToByte

codeTableToByteString :: [(Word8, [Bool])] -> ByteString
codeTableToByteString codes = pack $ (fromIntegral $ length word8s):word8s
    where entries = [(s :: Word8):(fromIntegral $ length enc):(unfoldr boolToByte enc) | (s, enc) <- codes]
          word8s = concat entries

codeTableFromBytes :: [Word8] ->[(Word8, [Bool])]
codeTableFromBytes bytes = helper bytes []
    where helper [] codes = codes
          helper (x:(y:(z:rest))) codes = (x, fst . splitAt (fromIntegral y) $ byteToBool z):(helper rest codes)
