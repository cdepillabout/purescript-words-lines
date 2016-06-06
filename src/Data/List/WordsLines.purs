
module Data.List.WordsLines where

import Prelude ((<>), not, (<<<), map, (==), ($))

import Data.List (List)
import Data.List as List
import Data.Char.Unicode (isSpace)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String as String

-- | `unwords` is an inverse operation to `words`.
-- | It joins words with separating spaces.
-- |
-- | However, `unwords <<< words` is not an isomorphism.
unwords :: List String -> String
unwords = foldl f ""
  where
    f :: String -> String -> String
    f "" a = a
    f acc a = acc <> " " <> a

-- | 'unlines' is an inverse operation to `lines`.
-- | It joins lines, after appending a terminating newline to each.
-- |
-- | However, `unlines <<< lines` is not an isomorphism.
unlines :: List String -> String
unlines = foldl f ""
  where
    f :: String -> String -> String
    f acc a = acc <> a <> "\n"

-- | `words` breaks a string up into a list of words, which were delimited
-- | by white space.
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

-- | `lines` breaks a string up into a list of strings at newline
-- | characters.  The resulting strings do not contain newlines.
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

-- | `break p as` is `span (not <<< p) as`.
break :: forall a . (a -> Boolean) -> List a -> { init :: (List a), rest :: (List a) }
break p = List.span (not <<< p)

-- | Changes a string to a `List` of `Char`s.  This is similar to
-- | `toCharArray`.
toCharList :: String -> List Char
toCharList s =
    case String.uncons s of
        Nothing -> List.Nil
        Just { head: head, tail: tail } ->
            head `List.Cons` toCharList tail

-- | Changes a `List` of `Char`s to a `String`. This is similar to
-- | `fromCharArray`.
fromCharList :: List Char -> String
fromCharList = foldl (\accum a -> accum <> String.singleton a) ""
