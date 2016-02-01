
module Test.Data.List.WordsLines (listTests) where

import Prelude (Unit, ($), bind)

import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

import Data.List (List(Nil, Cons))
import Data.List.WordsLines (unlines, lines, unwords, words)


listTests :: forall eff . Spec eff Unit
listTests = describe "Data.List.WordsLines" do
    describe "words" do
        it "words \"\"              == []"          $
            words ""              `shouldEqual` Nil
        it "words \"hello\"         == [\"hello\"]" $
            words "hello"         `shouldEqual` Cons "hello" Nil
        it "words \"hello     \"    == [\"hello\"]" $
            words "hello     "    `shouldEqual` Cons "hello" Nil
        it "words \"    hello\"     == [\"hello\"]" $
            words "    hello"     `shouldEqual` Cons "hello" Nil
        it "words \"    hello    \" == [\"hello\"]" $
            words "    hello    " `shouldEqual` Cons "hello" Nil

        it "words \" hello there \"    == [\"hello\", \"there\"]" $
            words " hello  there "   `shouldEqual` Cons "hello" (Cons "there" Nil)
        it "words \"\\nhello\\nthere\\n\" == [\"hello\", \"there\"]" $
            words "\nhello\nthere\n" `shouldEqual` Cons "hello" (Cons "there" Nil)

        it "words \"japanese　space　characters\\t\" == [\"japanese\", \"space\", \"characters\"]" $
            words "japanese　space　characters\t" `shouldEqual` Cons "japanese" (Cons "space" (Cons "characters" Nil))

    describe "unwords" do
        it "unwords []                      == \"\"" $
            unwords Nil                                          `shouldEqual` ""
        it "unwords [\"hello\"]               == \"hello\"" $
            unwords (Cons "hello" Nil)                           `shouldEqual` "hello"
        it "unwords [\"hello\", \" \", \"there\"] == \"hello   there\"" $
            unwords (Cons "hello" (Cons " " (Cons "there" Nil))) `shouldEqual` "hello   there"

    describe "lines" do
        it "lines \"\"                == []" $
            lines "" `shouldEqual` Nil
        it "lines \"hello there\"     == [\"hello there\"]" $
            lines "hello there" `shouldEqual` Cons "hello there" Nil
        it "lines \"\\n\"              == [\"\"]" $
            lines "\n" `shouldEqual` Cons "" Nil
        it "lines \"\\n\\n\\n\"          == [\"\", \"\", \"\"]" $
            lines "\n\n\n" `shouldEqual` Cons "" (Cons "" (Cons "" Nil))
        it "lines \"\\nfoo\\nbar\\nbaz\" == [\"\", \"foo\", \"bar\", \"baz\"]" $
            lines "\nfoo\nbar\nbaz" `shouldEqual` Cons "" (Cons "foo" (Cons "bar" (Cons "baz" Nil)))
        it "lines \"foo\\nbar\\nbaz\\n\" == [\"foo\", \"bar\", \"baz\"]" $
            lines "foo\nbar\nbaz\n" `shouldEqual` Cons "foo" (Cons "bar" (Cons "baz" Nil))

    describe "unlines" do
        it "unlines []                        == \"\"" $
            unlines Nil                      `shouldEqual` ""
        it "unlines [\"hello\"]                 == \"hello\\n\"" $
            unlines (Cons "hello" Nil)               `shouldEqual` "hello\n"
        it "unlines [\"hello\", \" \", \"there\"]   == \"hello\\n \\nthere\\n\"" $
            unlines (Cons "hello" (Cons " " (Cons "there" Nil))) `shouldEqual` "hello\n \nthere\n"
        it "unlines [\"hello\", \"\\n\", \"there\"]  == \"hello\\n\\n\\nthere\\n\"" $
            unlines (Cons "hello" (Cons "\n" (Cons "there" Nil))) `shouldEqual` "hello\n\n\nthere\n"
