{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures, FlexibleInstances #-}

{-|
Module : Objectspace
Description : Objectspace is the storage backend for Meera.
Copyright   : (c) Jess Balint, 2014, 2015
License     : TBD
Maintainer  : jbalint@gmail.com
Stability   : experimental

An object space is a context for storing objects. This module defines
the model for all stored objects.
-}

{--
* References:
  + https://github.com/kim/leveldb-haskell
  + https://hackage.haskell.org/package/cereal-0.3.0.0/docs/Data-Serialize.html
  + https://hackage.haskell.org/package/leveldb-haskell-0.6.2/docs/Data-Stream-Monadic.html
--}

module Objectspace (Id(Id), NsKey, ObjKey, PropKey, Fields, Obj(Obj),
                    Scalar(SId, SString, SInt, SRat, SBool, SAtom, SDate, SMethod, SList),
                    writeObject, readObject,
                    printDbObjects -- exported temporarily for debugging
                    ) where

import Data.ByteString
import Data.ByteString.UTF8
import Data.Default
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Serialize as Ser
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Database.LevelDB
import Database.LevelDB.MonadResource (get)

import GHC.Generics (Generic)

-- | A namespace is an optional string.
type NsKey = Maybe String

-- | A object key is a string that uniquely identifies an object in a
-- give namespace/context.
type ObjKey = String

-- | A property key identifies a slot of an object. It may or may not
-- specify a namespace key.
type PropKey = Id

{-|
An @Id@ is an identifier of an object. It consists of two string parts:
* An optional namespace key (via Maybe)
* An object key

@Id@s are used universally to identify objects globally. The namespace
key provides a context on how to resolve an object
reference. References are not enforced and there may name objects for
which no reference can be resolved because there is no mapping for the
context corresponding to the namespace key.
-}
data Id = Id NsKey ObjKey
        deriving (Show, Eq, Generic, Ord)

-- | A @Scalar@ is a value that can be stored as in slot of an object.
data Scalar = SId Id | -- object reference
              SString String |
              SInt Integer | -- Integer is arbitrary precision
              SRat Rational | -- Rational is arbitrary precision
              SBool Bool |
              SAtom String |
              SDate Day |
              STimestamp UTCTime |
              SList [Scalar] |
              SMethod [Scalar] Scalar
            deriving (Show, Eq, Generic, Ord)

-- | Hi Fields!
type Fields = Map.Map PropKey Scalar

{-|
An Object is a composite:
 // TODO: need reserved space for object-level annotations
-}
data Obj = Obj Id Fields
         deriving (Show, Eq, Generic)

instance Ser.Serialize Day where
  put = Ser.put . toGregorian
  get = do (y, m, d) <- Ser.get
           return $ fromGregorian y m d

instance Ser.Serialize UTCTime where
  put = Ser.put . (formatTime defaultTimeLocale "%s")
  get = Ser.get >>= return . (parseTimeOrError True defaultTimeLocale "%s")

instance Ser.Serialize Fields where
  put = Ser.put . Map.toList
  get = Ser.get >>= return . Map.fromList

instance Ser.Serialize Id
instance Ser.Serialize Scalar
instance Ser.Serialize Obj

{-|
Write an object.
-}
writeObject :: FilePath -> Obj -> IO ()
writeObject file obj@(Obj (Id (Just _) k) _) =
  let keyBytes = Data.ByteString.UTF8.fromString k
      objBytes = Ser.encode obj
  in runResourceT $ do
    db <- open file defaultOptions {createIfMissing = True}
    put db def keyBytes objBytes
    return ()

{-|
Read an object.
-}
readObject :: FilePath -> Id -> IO (Maybe Obj)
readObject file (Id Nothing _) = error "namespace must be supplied"
readObject file (Id (Just ns) k) =
  let encK = Data.ByteString.UTF8.fromString k
  in runResourceT $ do -- TODO: (optional) namespace validation?
    db <- open file defaultOptions
    obj <- get db defaultReadOptions encK
    return (case ((Ser.decode $ fromJust obj) :: Either String Obj) of Right x -> Just x)

writeSomeDbObjects :: IO ()
writeSomeDbObjects =  do
  curTime <- getCurrentTime
  runResourceT $ open "test.ospace" defaultOptions {createIfMissing = True}
  writeObject "test.ospace" .
    Obj (Id (Just "test") "jess") $
    Map.fromList [(Id Nothing "age", SInt 33),
                  (Id Nothing "likes_musician", SString "Philip Glass"),
                  (Id Nothing "knows", SId $ Id (Just "test") "andy"),
                  (Id Nothing "has_siblings_probability", SMethod [SRat 0.9] $ SBool True),
                  (Id Nothing "created", STimestamp curTime)]
  writeObject "test.ospace" .
    Obj (Id (Just "test") "andy") $
    Map.fromList [(Id Nothing "age", SString "<unknown>"),
                  (Id Nothing "likes_music_style", SString "Irish"),
                  -- this is like: andy:age_between(test:Knuth, test:Jess) => True
                  (Id Nothing "age_between", SMethod [SId $ Id (Just "test") "Knuth", SId $ Id (Just "test") "Jess"] $ SBool True)]
  -- from WordNet
  -- s(100002137,1,'abstraction',n,6,0).
  -- s(100002137,2,'abstract entity',n,1,0).
  writeObject "test.ospace" .
    Obj (Id (Just "test") "100002137") $
    Map.fromList [] -- TODO
  return ()

printDbObjects :: String -> IO ()
printDbObjects namespace =
  let getObjsFromIter :: MonadResource m => Iterator -> Bool -> m [(Maybe ByteString, Maybe ByteString)]
      getObjsFromIter i False = return []
      getObjsFromIter i True = do
        k <- iterKey i
        v <- iterValue i
        _ <- iterNext i
        valid <- iterValid i
        rest <- getObjsFromIter i valid
        return $ (k, v) : rest
  in do
  objs2 <- runResourceT $ do
    db <- open (namespace ++ ".ospace") defaultOptions
    withIterator db def (\i -> do
                            _ <- iterFirst i
                            valid <- iterValid i
                            getObjsFromIter i valid)
  mapM_ (\(k, v) -> print ((Ser.decode $ fromJust v) :: Either String Obj)) objs2
  return ()

main :: IO ()
main = do
  writeSomeDbObjects
  printDbObjects "test"
