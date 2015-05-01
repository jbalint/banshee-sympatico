{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

{--
    * References:
      + https://github.com/kim/leveldb-haskell
      + https://hackage.haskell.org/package/cereal-0.3.0.0/docs/Data-Serialize.html
--}

{--
An id is a tuple: NS:ID

A reference property is a tuple: NS:ID

A value is one of the following:
  - id : object reference
  - scalar : string, int, double, (TODO: date, list, etc)

An Object is a composite:
{id : id,
 zero or more:
 field : reference property | un-referenced property
 // TODO: need reserved space for object-level annotations
--}

module Main where

import Data.Default
import qualified Data.Map as Map
--import Data.Serialize
import qualified Data.Serialize as Ser
import Database.LevelDB
import GHC.Exts hiding (Any)
import GHC.Generics (Generic)

data Id = Id String String
        deriving (Show, Eq, Generic)

data Scalar = SId Id |
              SString String |
              SInt Integer | -- Integer is arbitrary precision
              SRat Rational |
              SBool Bool
            deriving (Show, Eq, Generic)

type Fields = Map.Map String Scalar

data Obj = Obj Id Fields
         deriving (Show, Eq, Generic)

instance Ser.Serialize Id
instance Ser.Serialize Scalar
instance Ser.Serialize Obj

writeSomeDbObjects :: IO ()
writeSomeDbObjects = runResourceT $ do
  db <- open "test.ospace"
        defaultOptions {createIfMissing = True}
  return ()
printDbObjects :: IO ()
printDbObjects = runResourceT $ do
  db <- open "test.ospace"
        defaultOptions
  return ()
  
main :: IO ()
-- main = runResourceT $ do
--   db <- open "/tmp/myFirstLevelDb.objs"
--         defaultOptions {createIfMissing = True}
--   put db def "zzz" ""
--   return ()
main = do
  printDbObjects
