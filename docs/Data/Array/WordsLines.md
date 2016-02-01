## Module Data.Array.WordsLines

#### `unwords`

``` purescript
unwords :: Array String -> String
```

`unwords` is an inverse operation to `words`.
It joins words with separating spaces.

However, `unlines <<< lines` is not an isomorphism.

```purescript
> unwords []
""
> unwords ["hello"]
"hello"
> unwords ["hello", " ", "there"]
"hello   there"
```

#### `unlines`

``` purescript
unlines :: Array String -> String
```

'unlines' is an inverse operation to `lines`.
It joins lines, after appending a terminating newline to each.

However, `unlines <<< lines` is not an isomorphism.

```purescript
> unlines []
""
> unlines ["hello"]
"hello\n"
> unlines ["hello", " ", "there"]
"hello\n \nthere\n"
> unlines ["hello", "\n", "there"]
"hello\n\n\nthere\n"
```

#### `words`

``` purescript
words :: String -> Array String
```

`words` breaks a string up into a list of words, which were delimited
by white space.

```purescript
> words ""
[""]
> words "hello"
["hello"]
> words "hello     "
["hello"]
> words "   foo     bar   "
["foo", "bar"]
> words "foo\t\nbar"
["foo", "bar"]
```

#### `lines`

``` purescript
lines :: String -> Array String
```

`lines` breaks a string up into a list of strings at newline
characters.  The resulting strings do not contain newlines.

```purescript
> lines "hello"
["hello"]
> lines "hello\nbye"
["hello", "bye"]
> lines "hello\nbye\n"
["hello", "bye"]
> lines "hello\n\nbye\n"
["hello", "", "bye"]
```

#### `break`

``` purescript
break :: forall a. (a -> Boolean) -> Array a -> { init :: Array a, rest :: Array a }
```

`break p as` is `span (not <<< p) as`.


