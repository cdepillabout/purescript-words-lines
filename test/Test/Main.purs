module Test.Main where

import Prelude (Unit, bind)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Test.Spec.Runner (Process, run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Data.Array.WordsLines (arrayTests)
import Test.Data.List.WordsLines (listTests)

main :: forall e. Eff (console :: CONSOLE, err :: EXCEPTION, process :: Process, random :: RANDOM | e) Unit
main = run [consoleReporter] do
    arrayTests
    listTests
