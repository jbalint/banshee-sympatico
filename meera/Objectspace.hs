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
import Data.Time.Clock
import Data.Time.Format
import Database.LevelDB

import GHC.Generics (Generic)

type NsKey = Maybe String
type ObjKey = String
type PropKey = Id

data Id = Id NsKey ObjKey
        deriving (Show, Eq, Generic, Ord)

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

type Fields = Map.Map PropKey Scalar

data Obj = Obj Id Fields
         deriving (Show, Eq, Generic)

instance Ser.Serialize Day where
  put = Ser.put . toGregorian
  get = do (y, m, d) <- Ser.get
           return $ fromGregorian y m d

instance Ser.Serialize UTCTime where
  put = Ser.put . (formatTime defaultTimeLocale "%s")
  get = Ser.get >>= return . (parseTimeOrError True defaultTimeLocale "%s")

instance Ser.Serialize Id
instance Ser.Serialize Scalar
instance Ser.Serialize Obj

writeObject :: Obj -> IO ()
writeObject obj =
  let (Obj (Id ns k) _) = obj
      keyBytes = Data.ByteString.UTF8.fromString k
      objBytes = Ser.encode obj
  in runResourceT $ do
    db <- open ((fromJust ns) ++ ".ospace") defaultOptions
    put db def keyBytes objBytes
    return ()

readObject :: Id -> IO (Maybe Obj)
readObject (Id ns k) = return Nothing

writeSomeDbObjects :: IO ()
writeSomeDbObjects =  do
  curTime <- getCurrentTime
  runResourceT $ open "test.ospace" defaultOptions {createIfMissing = True}
  writeObject .
    Obj (Id (Just "test") "jess") $
    Map.fromList [(Id Nothing "age", SInt 33),
                  (Id Nothing "likes_musician", SString "Philip Glass"),
                  (Id Nothing "knows", SId $ Id (Just "test") "andy"),
                  (Id Nothing "has_siblings_probability", SMethod [SRat 0.9] $ SBool True),
                  (Id Nothing "created", STimestamp curTime)]
  writeObject .
    Obj (Id (Just "test") "andy") $
    Map.fromList [(Id Nothing "age", SString "<unknown>"),
                  (Id Nothing "likes_music_style", SString "Irish"),
                  -- this is like: andy:age_between(test:Knuth, test:Jess) => True
                  (Id Nothing "age_between", SMethod [SId $ Id (Just "test") "Knuth", SId $ Id (Just "test") "Jess"] $ SBool True)]
  -- from WordNet
  -- s(100002137,1,'abstraction',n,6,0).
  -- s(100002137,2,'abstract entity',n,1,0).
  writeObject .
    Obj (Id (Just "test") "100002137") $
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
    db <- open "bibo.ospace"
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
