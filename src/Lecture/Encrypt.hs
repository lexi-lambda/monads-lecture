module Lecture.Encrypt (
  EncryptionKey,
  encrypt,
  decrypt
) where

import Data.Char

type EncryptionKey = Integer

encrypt :: EncryptionKey -> String -> String
encrypt = rotN

decrypt :: EncryptionKey -> String -> String
decrypt = rotN . (+ 26) . negate . (`mod` 26)

rotN :: Integer -> String -> String
rotN 0 str = str
rotN n str = rotN (n - 1) (rot1 str)

rot1 :: String -> String
rot1 = map rotC

rotC :: Char -> Char
rotC 'z' = 'a'
rotC 'Z' = 'A'
rotC c
  | isAlpha c = chr (ord c + 1)
  | otherwise = c
