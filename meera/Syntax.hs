{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Syntax (
  Id(Id), NsKey, ObjKey, PropKey, Fields, Obj(Obj),
  Term (TId, TVar, TString, TInt, TRat, TBool, TAtom, TDate, TTimestamp, TList, TMethod, TObject)
  ) where

import Data.ByteString.UTF8 as B
import qualified Data.Map as Map
import qualified Data.Serialize as Ser
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import GHC.Generics (Generic)

-- | A namespace is an optional string.
type NsKey = Maybe String

-- | A object key is a string that uniquely identifies an object in a
-- give namespace/context.
type ObjKey = String

{-|
An @Id@ is an identifier of an object. It consists of two string parts:
* An optional namespace key
* An object key

@Id@s are used throughout to identify objects globally. The namespace
key provides a context for how to resolve an object
reference. References are not enforced and there may named objects for
which no reference can be resolved. This happens if there is no
mapping for the context corresponding to the namespace key.
-}
data Id = Id NsKey ObjKey
        deriving (Show, Eq, Generic, Ord, Ser.Serialize)

-- | A property key identifies a slot of an object. It may or may not
-- specify a namespace key.
type PropKey = Id

-- | Hi Fields!
type Fields = Map.Map PropKey Term

data Term = TId Id | -- object reference
            TVar String | -- variable
            TString ByteString |
            TInt Integer | -- Integer is arbitrary precision
            TRat Rational | -- Rational is arbitrary precision
            TBool Bool |
            TAtom String |
            TDate Day |
            TTimestamp UTCTime |
            TList [Term] |
            TMethod [Term] Term | -- arguments -> value
            TObject Fields
          deriving (Show, Eq, Generic, Ord, Ser.Serialize)

{-|
An Object is a composite:
 // TODO: need reserved space for object-level annotations
-}
data Obj = Obj Id Fields
         deriving (Show, Eq, Generic, Ser.Serialize)

instance Ser.Serialize Day where
  put = Ser.put . toGregorian
  get = do (y, m, d) <- Ser.get
           return $ fromGregorian y m d

instance Ser.Serialize UTCTime where
  put = Ser.put . (formatTime defaultTimeLocale "%s")
  get = Ser.get >>= return . (parseTimeOrError True defaultTimeLocale "%s")
