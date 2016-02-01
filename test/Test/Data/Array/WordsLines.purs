
module Test.Data.Array.WordsLines (arrayTests) where

import Prelude (Unit, ($), bind)

import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

import Data.Array.WordsLines (unlines, lines, unwords, words)


arrayTests :: forall eff . Spec eff Unit
arrayTests = describe "Data.Array.WordsLines" do
    describe "words" do
        it "words \"\"              == []"          $
            words ""              `shouldEqual` []
        it "words \"hello\"         == [\"hello\"]" $
            words "hello"         `shouldEqual` ["hello"]
        it "words \"hello     \"    == [\"hello\"]" $
            words "hello     "    `shouldEqual` ["hello"]
        it "words \"    hello\"     == [\"hello\"]" $
            words "    hello"     `shouldEqual` ["hello"]
        it "words \"    hello    \" == [\"hello\"]" $
            words "    hello    " `shouldEqual` ["hello"]

        it "words \" hello there \"    == [\"hello\", \"there\"]" $
            words " hello  there "   `shouldEqual` ["hello", "there"]
        it "words \"\\nhello\\nthere\\n\" == [\"hello\", \"there\"]" $
            words "\nhello\nthere\n" `shouldEqual` ["hello", "there"]

        it "words \"japanese　space　characters\\t\" == [\"japanese\", \"space\", \"characters\"]" $
            words "japanese　space　characters\t" `shouldEqual` ["japanese", "space", "characters"]

    describe "unwords" do
        it "unwords []                      == \"\"" $
            unwords []                      `shouldEqual` ""
        it "unwords [\"hello\"]               == \"hello\"" $
            unwords ["hello"]               `shouldEqual` "hello"
        it "unwords [\"hello\", \" \", \"there\"] == \"hello   there\"" $
            unwords ["hello", " ", "there"] `shouldEqual` "hello   there"

    describe "lines" do
        it "lines \"\"                == []" $
            lines "" `shouldEqual` []
        it "lines \"hello there\"     == [\"hello there\"]" $
            lines "hello there" `shouldEqual` ["hello there"]
        it "lines \"\\n\"              == [\"\"]" $
            lines "\n" `shouldEqual` [""]
        it "lines \"\\n\\n\\n\"          == [\"\", \"\", \"\"]" $
            lines "\n\n\n" `shouldEqual` ["", "", ""]
        it "lines \"\\nfoo\\nbar\\nbaz\" == [\"\", \"foo\", \"bar\", \"baz\"]" $
            lines "\nfoo\nbar\nbaz" `shouldEqual` ["", "foo", "bar", "baz"]
        it "lines \"foo\\nbar\\nbaz\\n\" == [\"foo\", \"bar\", \"baz\"]" $
            lines "foo\nbar\nbaz\n" `shouldEqual` ["foo", "bar", "baz"]

    describe "unlines" do
        it "unlines []                        == \"\"" $
            unlines []                      `shouldEqual` ""
        it "unlines [\"hello\"]                 == \"hello\\n\"" $
            unlines ["hello"]               `shouldEqual` "hello\n"
        it "unlines [\"hello\", \" \", \"there\"]   == \"hello\\n \\nthere\\n\"" $
            unlines ["hello", " ", "there"] `shouldEqual` "hello\n \nthere\n"
        it "unlines [\"hello\", \"\\n\", \"there\"]  == \"hello\\n\\n\\nthere\\n\"" $
            unlines ["hello", "\n", "there"] `shouldEqual` "hello\n\n\nthere\n"
