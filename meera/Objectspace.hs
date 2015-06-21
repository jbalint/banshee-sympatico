{-# LANGUAGE OverloadedStrings, DeriveGeneric, DefaultSignatures #-}

{--
    * References:
      + https://github.com/kim/leveldb-haskell
      + https://hackage.haskell.org/package/cereal-0.3.0.0/docs/Data-Serialize.html
      + https://hackage.haskell.org/package/leveldb-haskell-0.6.2/docs/Data-Stream-Monadic.html
--}

{--
An id is a tuple: NS:ID

A reference property is a tuple: NS:ID

A value is one of the following:
  - id : object reference
  - scalar : string, int, double, date, list, etc

An Object is a composite:
{id : id,
 zero or more:
 field : reference property | un-referenced property
 // TODO: need reserved space for object-level annotations
--}

module Objectspace (Id(Id), Fields, Obj(Obj),
                    Scalar(SId, SString, SInt, SRat, SBool, SAtom, SDate, SMethod, SList),
                    writeObject, readObject) where

import Data.ByteString
import Data.ByteString.UTF8
import Data.Default
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Serialize as Ser
import Data.Time.Calendar
import Database.LevelDB

import GHC.Generics (Generic)

type NsKey = String
type ObjKey = String
type PropKey = String

data Id = Id NsKey ObjKey
        deriving (Show, Eq, Generic)

data Scalar = SId Id | -- object reference
              SString String |
              SInt Integer | -- Integer is arbitrary precision
              SRat Rational | -- Rational is arbitrary precision
              SBool Bool |
              SAtom String |
              SDate Day |
              SList [Scalar] |
              SMethod [Scalar] Scalar
            deriving (Show, Eq, Generic)

type Fields = Map.Map PropKey Scalar

data Obj = Obj Id Fields
         deriving (Show, Eq, Generic)

-- the Data.Time.Calendar.Day type is not serializable
instance Ser.Serialize Day where
  put = Ser.put . toGregorian
  get = do (y, m, d) <- Ser.get
           return $ fromGregorian y m d

instance Ser.Serialize Id
instance Ser.Serialize Scalar
instance Ser.Serialize Obj

writeObject :: Obj -> IO ()
writeObject obj =
  let (Obj (Id ns k) _) = obj
      keyBytes = Data.ByteString.UTF8.fromString k
      objBytes = Ser.encode obj
  in runResourceT $ do
    db <- open (ns ++ ".ospace") defaultOptions
    put db def keyBytes objBytes
    return ()

readObject :: Id -> IO (Maybe Obj)
readObject (Id ns k) = return Nothing

writeSomeDbObjects :: IO ()
writeSomeDbObjects =  do
  runResourceT $ open "test.ospace" defaultOptions {createIfMissing = True}
  writeObject .
    Obj (Id "test" "jess") $
    Map.fromList [("age", SInt 33),
                  ("likes_musician", SString "Philip Glass"),
                  ("knows", SId $ Id "test" "andy"),
                  ("has_siblings_probability", SMethod [SRat 0.9] $ SBool True)]
  writeObject .
    Obj (Id "test" "andy") $
    Map.fromList [("age", SString "<unknown>"),
                  ("likes_music_style", SString "Irish"),
                  -- this is like: andy:age_between(test:AndyFather, test:Jess) => True
                  ("age_between", SMethod [SId $ Id "test" "AndyFather", SId $ Id "test" "Jess"] $ SBool True)]
  -- from WordNet
  -- s(100002137,1,'abstraction',n,6,0).
  -- s(100002137,2,'abstract entity',n,1,0).
  writeObject .
    Obj (Id "test" "100002137") $
    Map.fromList [] -- TODO
  return ()

printDbObjects :: IO ()
printDbObjects =
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
    db <- open "test.ospace"
          defaultOptions
    withIterator db def (\i -> do
                            _ <- iterFirst i
                            valid <- iterValid i
                            getObjsFromIter i valid)
  mapM_ (\(k, v) -> print ((Ser.decode $ fromJust v) :: Either String Obj)) objs2
  return ()

main :: IO ()
main = do
  writeSomeDbObjects
  printDbObjects
