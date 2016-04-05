{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor #-}

module Syntax (
  Id(Id), NsKey, ObjKey, PropKey, Fields, Obj(Obj),
  Term (TId, TVar, TString, TInt, TRat, TBool, TAtom, TDate, TTimestamp, TList, TMethod, TFunction, TObject),
  Pred(Pred, NegPred), Rule(Rule), Clause
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

{-|
A @Term@ represents any value in the system. Some examples:

skos#Collection[rdf#type -> owl#Class].
TMethod (TId $ Id (Just "skos") "Collection") (TId $ Id (Just "rdf") "type") [] (TId $ Id (Just "owl") "Class")
-}

data Symbol = SymVar ByteString |
              SymConst ByteString

data Oid = Oid Id

{-
1. O[M -> V], C[|M -> V|]
2. O[M -> {V1, ..., Vn}], C[|M -> {V1, ..., Vn}|]
3. O[M => T], C[|M => T|], C[|M{L..H} => T|]
4. O[V], C[|V|]
5. O[ => T], C[| => T|]
6. O[ ], C[| |]
-}
data Term = TId Id | -- object reference
            TVar ByteString | -- variable
            TString ByteString |
            TInt Integer | -- Integer is arbitrary precision
            TRat Rational | -- Rational is arbitrary precision
            TBool Bool |
            TAtom String |
            TDate Day |
            TTimestamp UTCTime |
            TList [Term] |
            TFunction ByteString [Term] |
            -- TODO: is this right? it works as a value for Fields
            -- but how do I unify with it? X->method(A, B) = C?
            -- Seems ok
            TMethod Term Term [Term] Term | -- obj -> method -> arguments -> value
            TObject Obj
          deriving (Show, Eq, Generic, Ord, Ser.Serialize)

type X = Term

myFmap :: (X -> a) -> X -> a
myFmap f v@(TInt _) = f v
--myFmap f (TList xs) = fmap f xs

addOne :: X -> X
addOne (TInt i) = TInt (i + 1)

apply1 = myFmap addOne (TInt 4)
apply2 = myFmap addOne (TList [TInt 0, TInt 1])

-- transform terms
-- myFmap :: (Term -> Term) -> Term -> Term
-- myFmap f (TList ts) = TList (map f ts)
-- myFmap f (TFunction name args) = TFunction name (map f args)
-- myFmap f (TMethod args v) = TMethod (map f args) (f v)
-- myFmap f t = f t

{-|
An Object is a composite:
 // TODO: need reserved space for object-level annotations
-}
data Obj = Obj Id Fields
         deriving (Show, Eq, Generic, Ord, Ser.Serialize)

instance Ser.Serialize Day where
  put = Ser.put . toGregorian
  get = do (y, m, d) <- Ser.get
           return $ fromGregorian y m d

instance Ser.Serialize UTCTime where
  put = Ser.put . (formatTime defaultTimeLocale "%s")
  get = Ser.get >>= return . (parseTimeOrError True defaultTimeLocale "%s")


data Pred = Pred ByteString [Term] | NegPred ByteString [Term]
          deriving (Show, Eq)

type Clause = [Pred]

data Rule = Rule Pred Clause
          deriving (Show, Eq)

-- really need to represent more here a[b->c] :- f(a).
