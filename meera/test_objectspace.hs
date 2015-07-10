module Main where

import qualified Data.Map as Map
import Data.Time.Calendar

import Objectspace

jess2 =
    Obj (Id (Just "test") "jess2") $
    Map.fromList [(Id Nothing "doppleganger_of", SId $ Id (Just "test") "jess"),
                  (Id Nothing "birthday", SDate $ fromGregorian 1982 4 6)]

biboBook =
  Obj (Id (Just "bibo") "book") $
  Map.fromList [(Id (Just "rdf") "type", SId $ Id (Just "owl") "Class"),
                (Id (Just "rdfs") "comment", SString "A written or printed work of fiction or nonfiction, usually on sheets of paper fastened or bound together within covers."),
                -- TODO could put an atomic URI type in the system
                (Id (Just "rdfs") "label", SString "Book"),
                (Id (Just "rdfs") "subClassOf", SId $ Id (Just "bibo") "Document"),
                (Id (Just "ns") "term_status", SString "stable")]

main :: IO ()
main =
  writeObject jess2
