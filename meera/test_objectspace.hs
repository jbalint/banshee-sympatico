module Main where

import qualified Data.Map as Map
import Data.Time.Calendar

import Objectspace

jess2 =
    Obj (Id "test" "jess2") $
    Map.fromList [("doppleganger_of", SId $ Id "test" "jess"),
                  ("birthday", SDate $ fromGregorian 1982 4 6)]

main :: IO ()
main =
  writeObject jess2
