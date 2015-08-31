{-# LANGUAGE OverloadedStrings #-}

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

module Objectspace (writeObject, readObject,
                    printDbObjects -- exported temporarily for debugging
                    ) where

import qualified Data.ByteString.UTF8 as B (ByteString, fromString)
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import Data.Serialize as Ser hiding (put, get)
import Data.Time.Clock
import Database.LevelDB
import Database.LevelDB.MonadResource (get)
import GHC.Generics (Generic)

import Syntax

{-|
Write an object.
-}
writeObject :: FilePath -> Obj -> IO ()
writeObject file obj@(Obj (Id (Just _) k) _) =
  let keyBytes = B.fromString k
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
  let encK = B.fromString k
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
    Map.fromList [(Id Nothing "age", TInt 33),
                  (Id Nothing "likes_musician", TString "Philip Glass"),
                  (Id Nothing "knows", TId $ Id (Just "test") "andy"),
                  (Id Nothing "has_siblings_probability", TMethod [TRat 0.9] $ TBool True),
                  (Id Nothing "created", TTimestamp curTime)]
  writeObject "test.ospace" .
    Obj (Id (Just "test") "andy") $
    Map.fromList [(Id Nothing "age", TString "<unknown>"),
                  (Id Nothing "likes_music_style", TString "Irish"),
                  -- this is like: andy:age_between(test:Knuth, test:Jess) => True
                  (Id Nothing "age_between", TMethod [TId $ Id (Just "test") "Knuth", TId $ Id (Just "test") "Jess"] $ TBool True)]
  -- from WordNet
  -- s(100002137,1,'abstraction',n,6,0).
  -- s(100002137,2,'abstract entity',n,1,0).
  writeObject "test.ospace" .
    Obj (Id (Just "test") "100002137") $
    Map.fromList [] -- TODO
  return ()

printDbObjects :: String -> IO ()
printDbObjects namespace =
  let getObjsFromIter :: MonadResource m => Iterator -> Bool -> m [(Maybe B.ByteString, Maybe B.ByteString)]
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
