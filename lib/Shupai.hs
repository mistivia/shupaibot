module Shupai 
  ( shupai
  )
where

import Data.Bits ((.&.), complement)
import Data.Char (ord)
import qualified Data.Map as Map
import Data.Function ((&))
import qualified Data.Maybe as Maybe
import Data.List.Split (chunksOf)
import Data.List (transpose)


isWideChar :: Char -> Bool
isWideChar c = inRange $ ord c where
  inRange x
    | x >= 0x1100 && x <= 0x115f = True
    | x >= 0x115f && x <= 0xa4cf && x /= 0x303f
        && (x .&. complement 0x0011) /= 0x300a = True
    | x >= 0xac00 && x <= 0xd7a3 = True
    | x >= 0xf900 && x <= 0xfaff = True
    | x >= 0xfe30 && x <= 0xfe6f = True
    | x >= 0xff00 && x <= 0xff5f = True
    | x >= 0xffe0 && x <= 0xffe6 = True
    | x >= 0x20000 && x <= 0x2ffff = True
    | otherwise = False

textPreprocess :: String -> String
textPreprocess s = s & filter isValid & map toFullWidth where
  isValid c = isWideChar c || Map.member c fullWidthMap
  toFullWidth c = Maybe.fromMaybe c $ Map.lookup c fullWidthMap

verticalFormat :: String -> String
verticalFormat s 
  | length s < 25 = verticalCompose $ chunksOf 5 s
  | otherwise = verticalCompose $ chunksOf (length s `div` 5 + 1) s

verticalCompose :: [String] -> String
verticalCompose lst = lst
  & fillSpace
  & transpose
  & map (addSpace . reverse)
  & unlines

addSpace :: String -> String
addSpace [] = []
addSpace [x] = [x]
addSpace (x:xs) = x : ' ' : addSpace xs

fillSpace :: [String] -> [String]
fillSpace [] = []
fillSpace [x] = [x]
fillSpace ls = go ls $ length (head ls) where
  go :: [String] -> Int -> [String]
  go [] _ = []
  go [x] len = [x ++ replicate (len - length x) 'ㅤ']
  go (x:xs) len = x : go xs len
    

shupai :: String -> String
shupai = verticalFormat . textPreprocess

fullWidthMap :: Map.Map Char Char
fullWidthMap = Map.fromList
  [ ('!', '！')
  , ('"', '＂')
  , ('#', '＃')
  , ('$', '＄')
  , ('%', '％')
  , ('&', '＆')
  , ('\'', '＇')
  , ('(', '（')
  , (')', '）')
  , ('*', '＊')
  , ('+', '＋')
  , (',', '，')
  , ('-', '－')
  , ('.', '．')
  , ('/', '／')
  , ('0', '０')
  , ('1', '１')
  , ('2', '２')
  , ('3', '３')
  , ('4', '４')
  , ('5', '５')
  , ('6', '６')
  , ('7', '７')
  , ('8', '８')
  , ('9', '９')
  , (':', '：')
  , (';', '；')
  , ('<', '＜')
  , ('=', '＝')
  , ('>', '＞')
  , ('?', '？')
  , ('@', '＠')
  , ('A', 'Ａ')
  , ('B', 'Ｂ')
  , ('C', 'Ｃ')
  , ('D', 'Ｄ')
  , ('E', 'Ｅ')
  , ('F', 'Ｆ')
  , ('G', 'Ｇ')
  , ('H', 'Ｈ')
  , ('I', 'Ｉ')
  , ('J', 'Ｊ')
  , ('K', 'Ｋ')
  , ('L', 'Ｌ')
  , ('M', 'Ｍ')
  , ('N', 'Ｎ')
  , ('O', 'Ｏ')
  , ('P', 'Ｐ')
  , ('Q', 'Ｑ')
  , ('R', 'Ｒ')
  , ('S', 'Ｓ')
  , ('T', 'Ｔ')
  , ('U', 'Ｕ')
  , ('V', 'Ｖ')
  , ('W', 'Ｗ')
  , ('X', 'Ｘ')
  , ('Y', 'Ｙ')
  , ('Z', 'Ｚ')
  , ('[', '［')
  , ('\\', '＼')
  , (']', '］')
  , ('^', '＾')
  , ('_', '＿')
  , ('`', '｀')
  , ('a', 'ａ')
  , ('b', 'ｂ')
  , ('c', 'ｃ')
  , ('d', 'ｄ')
  , ('e', 'ｅ')
  , ('f', 'ｆ')
  , ('g', 'ｇ')
  , ('h', 'ｈ')
  , ('i', 'ｉ')
  , ('j', 'ｊ')
  , ('k', 'ｋ')
  , ('l', 'ｌ')
  , ('m', 'ｍ')
  , ('n', 'ｎ')
  , ('o', 'ｏ')
  , ('p', 'ｐ')
  , ('q', 'ｑ')
  , ('r', 'ｒ')
  , ('s', 'ｓ')
  , ('t', 'ｔ')
  , ('u', 'ｕ')
  , ('v', 'ｖ')
  , ('w', 'ｗ')
  , ('x', 'ｘ')
  , ('y', 'ｙ')
  , ('z', 'ｚ')
  , ('{', '｛')
  , ('|', '｜')
  , ('}', '｝')
  , ('~', '～')
  , (' ', 'ㅤ')
  ]
