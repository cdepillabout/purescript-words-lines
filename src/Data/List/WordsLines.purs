
module Data.List.WordsLines where

import Prelude ((<>), not, (<<<), map, (==), ($))

import Data.List (List)
import Data.List as List
import Data.Char.Unicode (isSpace)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String as String

unwords :: List String -> String
unwords = foldl f ""
  where
    f :: String -> String -> String
    f "" a = a
    f acc a = acc <> " " <> a

unlines :: List String -> String
unlines = foldl f ""
  where
    f :: String -> String -> String
    f acc a = acc <> a <> "\n"

words :: String -> List String
words = map fromCharList <<< go <<< toCharList
  where
    go :: List Char -> List (List Char)
    go s =
        case List.uncons $ List.dropWhile isSpace s of
            Nothing -> List.Nil
            Just { head: head, tail: tail } ->
                let withBreaks = break isSpace (head `List.Cons` tail)
                in withBreaks.init `List.Cons` go withBreaks.rest

lines :: String -> List String
lines = map fromCharList <<< go <<< toCharList
  where
    go :: List Char -> List (List Char)
    go s =
        case List.uncons s of
            Nothing -> List.Nil
            Just _ ->
                let withBreaks = break (== '\n') s
                in withBreaks.init `List.Cons` go (stripNewLine withBreaks.rest)

    stripNewLine :: List Char -> List Char
    stripNewLine s =
        case List.uncons s of
             Nothing -> List.Nil
             Just { head: '\n', tail: tail } -> tail
             Just { head: head, tail: tail } -> head `List.Cons` tail

break :: forall a . (a -> Boolean) -> List a -> { init :: (List a), rest :: (List a) }
break p = List.span (not <<< p)

toCharList :: String -> List Char
toCharList s =
    case String.uncons s of
        Nothing -> List.Nil
        Just { head: head, tail: tail } ->
            head `List.Cons` toCharList tail

fromCharList :: List Char -> String
fromCharList = foldl (\accum a -> accum <> String.singleton a) ""
