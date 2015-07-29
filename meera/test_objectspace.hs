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

rdfAlt =
  Obj (Id (Just "rdf") "Alt") $
  Map.fromList [(Id (Just "rdf") "type", SId $ Id (Just "rdf") "Class"),
                (Id (Just "rdfs") "comment", SString "The class of containers of alternatives."),
                (Id (Just "rdfs") "isDefinedBy", SString "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"),
                (Id (Just "rdfs") "label", SString "Alt"),
                (Id (Just "rdfs") "subClassOf", SId $ Id (Just "rdfs") "Container")]

rdfBag =
  Obj (Id (Just "rdf") "Bag") $
  Map.fromList [(Id (Just "rdf") "type", SId $ Id (Just "rdfs") "Class"),
                (Id (Just "rdfs") "comment", SString "The class of unordered containers."),
                (Id (Just "rdfs") "isDefinedBy", SString "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"),
                (Id (Just "rdfs") "label", SString "Bag"),
                (Id (Just "rdfs") "subClassOf", SId $ Id (Just "rdfs") "Container")]

rdfType =
  Obj (Id (Just "rdf") "type") $
  Map.fromList [(Id (Just "rdf") "type", SId $ Id (Just "rdf") "Property"),
                (Id (Just "rdfs") "comment", SString "The subject is an instance of a class."),
                (Id (Just "rdfs") "domain", SId $ Id (Just "rdfs") "Resource"),
                -- TODO: we should have a URL type?
                (Id (Just "rdfs") "isDefinedBy", SString "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>"),
                (Id (Just "rdfs") "label", SString "type"),
                (Id (Just "rdfs") "range", SId $ Id (Just "rdfs") "Class")]

rdfsClass =
  Obj (Id (Just "rdfs") "Class") $
  Map.fromList [(Id (Just "rdf") "type", SId $ Id (Just "rdfs") "Class"),
                (Id (Just "rdfs") "comment", SString "The class of classes."),
                (Id (Just "rdfs") "isDefinedBy", SString "<http://www.w3.org/2000/01/rdf-schema#>"),
                (Id (Just "rdfs") "label", SString "Class"),
                (Id (Just "rdfs") "subClassOf", SId $ Id (Just "rdfs") "Resource")]

rdfsComment =
  Obj (Id (Just "rdfs") "comment") $
  Map.fromList [(Id (Just "rdf") "type", SId $ Id (Just "rdf") "Property"),
                (Id (Just "rdfs") "comment", SString "A description of the subject resource."),
                (Id (Just "rdfs") "domain", SId $ Id (Just "rdfs") "Resource"),
                (Id (Just "rdfs") "isDefinedBy", SString "<http://www.w3.org/2000/01/rdf-schema#>"),
                (Id (Just "rdfs") "label", SString "comment"),
                (Id (Just "rdfs") "range", SId $ Id (Just "rdfs") "Literal")]

rdfsDomain =
  Obj (Id (Just "rdfs") "domain") $
  Map.fromList [(Id (Just "rdf") "type", SId $ Id (Just "rdf") "Property"),
                (Id (Just "rdfs") "comment", SString "A domain of the subject property."),
                (Id (Just "rdfs") "domain", SId $ Id (Just "rdf") "Property"),
                (Id (Just "rdfs") "isDefinedBy", SString "<http://www.w3.org/2000/01/rdf-schema#>"),
                (Id (Just "rdfs") "label", SString "domain"),
                (Id (Just "rdfs") "range", SId $ Id (Just "rdfs") "Class")]

rdfsIsDefinedBy =
  Obj (Id (Just "rdfs") "isDefinedBy") $
  Map.fromList [(Id (Just "rdf") "type", SId $ Id (Just "rdf") "Property"),
                (Id (Just "rdfs") "comment", SString "The defininition of the subject resource."),
                (Id (Just "rdfs") "domain", SId $ Id (Just "rdfs") "Resource"),
                (Id (Just "rdfs") "isDefinedBy", SString "<http://www.w3.org/2000/01/rdf-schema#>"),
                (Id (Just "rdfs") "label", SString "isDefinedBy"),
                (Id (Just "rdfs") "range", SId $ Id (Just "rdfs") "Resource"),
                (Id (Just "rdfs") "subPropertyOf", SId $ Id (Just "rdfs") "seeAlso")]

main :: IO ()
main =
  writeObject jess2
