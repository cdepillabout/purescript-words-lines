
module Data.Array.WordsLines where

import Prelude (not, (<<<), map, (==), ($), (<>))

import Data.Array as Array
import Data.Char.Unicode (isSpace)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String as String

-- | `unwords` is an inverse operation to `words`.
-- | It joins words with separating spaces.
-- |
-- | However, `unwords <<< words` is not an isomorphism.
-- |
-- | ```purescript
-- | > unwords []
-- | ""
-- | > unwords ["hello"]
-- | "hello"
-- | > unwords ["hello", " ", "there"]
-- | "hello   there"
-- | ```
unwords :: Array String -> String
unwords = foldl f ""
  where
    f :: String -> String -> String
    f "" a = a
    f acc a = acc <> " " <> a

-- | 'unlines' is an inverse operation to `lines`.
-- | It joins lines, after appending a terminating newline to each.
-- |
-- | However, `unlines <<< lines` is not an isomorphism.
-- |
-- | ```purescript
-- | > unlines []
-- | ""
-- | > unlines ["hello"]
-- | "hello\n"
-- | > unlines ["hello", " ", "there"]
-- | "hello\n \nthere\n"
-- | > unlines ["hello", "\n", "there"]
-- | "hello\n\n\nthere\n"
-- | ```
unlines :: Array String -> String
unlines = foldl f ""
  where
    f :: String -> String -> String
    f acc a = acc <> a <> "\n"

-- | `words` breaks a string up into a list of words, which were delimited
-- | by white space.
-- |
-- | ```purescript
-- | > words ""
-- | [""]
-- | > words "hello"
-- | ["hello"]
-- | > words "hello     "
-- | ["hello"]
-- | > words "   foo     bar   "
-- | ["foo", "bar"]
-- | > words "foo\t\nbar"
-- | ["foo", "bar"]
-- | ```
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

-- | `lines` breaks a string up into a list of strings at newline
-- | characters.  The resulting strings do not contain newlines.
-- |
-- | ```purescript
-- | > lines "hello"
-- | ["hello"]
-- | > lines "hello\nbye"
-- | ["hello", "bye"]
-- | > lines "hello\nbye\n"
-- | ["hello", "bye"]
-- | > lines "hello\n\nbye\n"
-- | ["hello", "", "bye"]
-- | ```
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

-- | `break p as` is `span (not <<< p) as`.
break :: forall a . (a -> Boolean) -> Array a -> { init :: (Array a), rest :: (Array a) }
break p = Array.span (not <<< p)
