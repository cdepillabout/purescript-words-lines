## Module Data.List.WordsLines

#### `unwords`

``` purescript
unwords :: List String -> String
```

`unwords` is an inverse operation to `words`.
It joins words with separating spaces.

However, `unlines <<< lines` is not an isomorphism.

#### `unlines`

``` purescript
unlines :: List String -> String
```

'unlines' is an inverse operation to `lines`.
It joins lines, after appending a terminating newline to each.

However, `unlines <<< lines` is not an isomorphism.

#### `words`

``` purescript
words :: String -> List String
```

`words` breaks a string up into a list of words, which were delimited
by white space.

#### `lines`

``` purescript
lines :: String -> List String
```

`lines` breaks a string up into a list of strings at newline
characters.  The resulting strings do not contain newlines.

#### `break`

``` purescript
break :: forall a. (a -> Boolean) -> List a -> { init :: List a, rest :: List a }
```

`break p as` is `span (not <<< p) as`.

#### `toCharList`

``` purescript
toCharList :: String -> List Char
```

Changes a string to a `List` of `Char`s.  This is similar to

#### `fromCharList`

``` purescript
fromCharList :: List Char -> String
```

Changes a `List` of `Char`s to a `String`. This is similar to


