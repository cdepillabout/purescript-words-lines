
module Data.Array.WordsLines where

import Prelude (not, (<<<), map, (==), ($), (<>))

import Data.Array as Array
import Data.Char.Unicode (isSpace)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String as String

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

words :: String -> Array String
words = map String.fromCharArray <<< go <<< String.toCharArray
  where
    go :: Array Char -> Array (Array Char)
    go s =
        case Array.uncons $ Array.dropWhile isSpace s of
            Nothing -> []
            Just { head: head, tail: tail } ->
                let withBreaks = break isSpace (head `Array.cons` tail)
                in withBreaks.init `Array.cons` go withBreaks.rest

lines :: String -> Array String
lines = map String.fromCharArray <<< go <<< String.toCharArray
  where
    go :: Array Char -> Array (Array Char)
    go s =
        case Array.uncons s of
            Nothing -> []
            Just _ ->
                let withBreaks = break (== '\n') s
                in withBreaks.init `Array.cons` go (stripNewLine withBreaks.rest)

    stripNewLine :: Array Char -> Array Char
    stripNewLine s =
        case Array.uncons s of
             Nothing -> []
             Just { head: '\n', tail: tail } -> tail
             Just { head: head, tail: tail } -> head `Array.cons` tail

break :: forall a . (a -> Boolean) -> Array a -> { init :: (Array a), rest :: (Array a) }
break p = Array.span (not <<< p)
