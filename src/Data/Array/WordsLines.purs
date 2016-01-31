
module Data.Array.WordsLines where

import Prelude

import Data.Foldable

unwords :: Array String -> String
unwords = foldl f ""
  where
    f :: String -> String -> String
    f "" a = a
    f acc a = acc <> " " <> a

unlines :: Array String -> String
unlines = foldl f ""
  where
    f :: String -> String -> String
    f acc a = acc <> a <> "\n"
