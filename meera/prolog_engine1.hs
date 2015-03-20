-- prolog engine v1
-- TODO lists and strings

import Prelude hiding (pred)

import Test.Hspec

import Text.ParserCombinators.Parsec
import Text.Parsec.Error

import Control.Applicative hiding ((<|>), many)
import Control.Monad

import System.IO

import Data.List
import qualified Data.Map as Map

--import Debug.Trace (trace)

data Term = Atom String | Var String | Func String [Term] | Int Integer
          deriving (Show, Eq)

data Pred = Pred String [Term]
          deriving (Show, Eq)

data Rule = Rule Pred [Pred]
          deriving (Show, Eq)

-- substitution Var -> Term
type Sub = Map.Map String Term

-- ====== --
-- Parser --
-- ====== --

lowerWordP :: Parser String
lowerWordP = (:) <$> lower <*> many alphaNum

atomP :: Parser Term
atomP = Atom <$> lowerWordP

varP :: Parser Term
varP = liftM Var $ (:) <$> upper <*> many alphaNum

intP :: Parser Term
intP = Int . read <$> many1 digit

funcP :: Parser Term
funcP = Func <$>
        ((lowerWordP <|> string ".") <* char '(') <*>
        (termListP <* char ')')

termP :: Parser Term
termP = try funcP <|> intP <|> varP <|> atomP

-- interesting approach for just a list of terms, "return" here acts
-- as the list constructor

-- termListP = return <$> termP

termListP :: Parser [Term]
-- left-recursive version doesn't work
-- termListP = ((++) <$> termListP <* char ',' <*> (return <$> termP)) <|>
--             return <$> termP
termListP = sepBy termP (char ',' <* spaces)

predP :: Parser Pred
predP = Pred <$> lowerWordP <* char '(' <*> termListP <* char ')'

predListP :: Parser [Pred]
predListP = sepBy predP (char ',' <* spaces)

queryP :: Parser [Pred]
queryP = predListP <* char '.'

ruleP :: Parser Rule
ruleP = Rule <$> predP <* spaces <* string ":-" <* spaces <*> predListP <* char '.'

-- =========== --
-- Unification --
-- =========== --

-- handbook of tableau methods (pg 160)


-- purposely don't handle term lists of different lengths. will throw an exception at runtime
-- should probably handle functors with different arity here as they will never unify anyways
disagreement :: [Term] -> [Term] -> Maybe (Term, Term)
disagreement [] [] = Nothing
disagreement (x:xs) (y:ys)
  | x == y = disagreement xs ys
disagreement (Func f1 args1 : _) (Func f2 args2 : _)
  -- decomposition of functors, agreeing functors handled above
  | f1 == f2 = disagreement args1 args2
-- make sure the variable comes first if there is one
disagreement (x:_) (y:_) = Just $ case x of Var _ -> (x, y)
                                            _ -> (y, x)

applySub :: [Term] -> Sub -> [Term]
applySub [] _ = []
applySub (t@(Var v):ts) sub = newT : applySub ts sub
  where newT = case Map.lookup v sub of
                Just x -> x
                _ -> t
applySub (Func f args : ts) sub = Func f (applySub args sub) : applySub ts sub
applySub (t:ts) sub = t : applySub ts sub

occursCheck :: Term -> String -> Bool
occursCheck (Atom _) _ = False
occursCheck (Func _ args) vname = Var vname `elem` args
occursCheck _ _ = False

unify :: [Term] -> [Term] -> Maybe Sub
unify = let unifyInternal s t1 t2 =
              let d = disagreement t1 t2 in
               case d of
                Nothing -> Just s
                Just (Var vname, t) | not $ occursCheck t vname -> unifyInternal s' t1' t2'
                  where s' = Map.insert vname t s
                        t1' = applySub t1 s'
                        t2' = applySub t2 s'
                _ -> Nothing
        in unifyInternal Map.empty

-- unify [Func "vertical" [Func "line" [Func "point" [Var "X", Var "Y"], Func "point" [Var "X", Var "Z"]]]] [Func "vertical" [Func "line" [Func "point" [Var "X", Var "Y"], Func "point" [Var "X", Var "Z"]]]]

-- TODO add these to tests:
{--
*Main> unify [Func "vertical" [Func "line" [Func "point" [Var "X", Var "Y"], Func "point" [Var "X", Var "Z"]]]] [Func "vertical" [Func "line" [Func "point" [Int 1, Int 1], Func "point" [Int 1, Int 3]]]]
Just (fromList [("X",Int 1),("Y",Int 1),("Z",Int 3)])
*Main> unify [Func "vertical" [Func "line" [Func "point" [Var "X", Var "Y"], Func "point" [Var "X", Var "Z"]]]] [Func "vertical" [Func "line" [Func "point" [Int 1, Int 1], Func "point" [Int 3, Int 2]]]]
Nothing
--}
-- ========================== --
-- repl stuff copied from:
-- ========================== --
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = --return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)
  return $ case parse (queryP <* eof) "" expr
           of Right x -> show x
              -- TODO this error message isn't useful
              Left err -> intercalate "\n" (map messageString $ errorMessages err)
              --Left err -> foldl (\x y -> messageString x ++ y (Message "") $ errorMessages err

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   unless (pred result) $ action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "?- ") evalAndPrint

-- ===============================
main :: IO ()
main = do
  putStrLn "==============="
  putStrLn "Prolog Engine 1"
  putStrLn "==============="
  hspec . describe ">>> Parser tests" $ do
    it "should parse basic predicates" $
      case parse (predP <* eof) "" "pred(a, B)"
      of Right x -> x `shouldBe`
                    Pred "pred" [Atom "a", Var "B"]
    it "should parse functors" $
      case parse (predP <* eof) "" "pred(sentence(nphrase(john),vbphrase(verb(likes),nphrase(mary))), B)"
      of Right x -> x `shouldBe`
                    Pred "pred"
                    [Func "sentence"
                     [Func "nphrase"
                      [Atom "john"],
                      Func "vbphrase"
                      [Func "verb"
                       [Atom "likes"],
                       Func "nphrase"
                       [Atom "mary"]]],
                     Var "B"]
    it "should parse rules" $
      case parse (ruleP <* eof) "" "jealous(X, Y) :- loves(X, Z), loves(Y, Z)."
      of Right x -> x `shouldBe`
                    Rule (Pred "jealous" [Var "X",Var "Y"])
                    [Pred "loves" [Var "X",Var "Z"],
                     Pred "loves" [Var "Y",Var "Z"]]
    it "should parse unsugared lists" $
      -- TODO last param should be the empty list
      case parse (termP <* eof) "" ".(1, .(2, .(3, .())))"
      of Right x -> x `shouldBe`
                    Func "." [Int 1,Func "." [Int 2,Func "." [Int 3,Func "." []]]]
  hspec . describe ">>> Unification tests" $ do
    it "should calculate disagreement sets properly" $
      let dset = disagreement
                 [Func "g" [Atom "x"], Atom "y"]
                 [Func "g" [Atom "a", Atom "y", Atom "u"]] in
      dset `shouldBe` Just (Atom "a",Atom "x")
    it "should calculate disagreement sets with functors properly" $
      let dset = disagreement
                 [Func "g" [Atom "x"], Atom "y"]
                 [Func "g" [Atom "x"], Atom "n", Atom "u"] in
      dset `shouldBe` Just (Atom "n",Atom "y")
    it "should apply a substitution" $
      let sub = Map.fromList [("X",Atom "a")]
          expr1 = Func "f" [Atom "b", Var "X"]
          expr2 = Func "f" [Func "g" [Atom "a"]]
          expr1' = applySub [expr1] sub
          expr2' = applySub [expr2] sub
      in (expr1', expr2') `shouldBe`
         ([Func "f" [Atom "b",Atom "a"]], [Func "f" [Func "g" [Atom "a"]]])
    it "should perform a trivial unification" $
      unify [Var "X"] [Atom "a"] `shouldBe` Just (Map.fromList [("X",Atom "a")])
    it "should reject an invalid unification" $
      unify [Atom "b"] [Func "f" [Var "X"]] `shouldBe` Nothing
    it "should unify within functors" $
      unify
      [Atom "a", Func "f" [Atom "a", Var "X"]]
      [Atom "a", Func "f" [Atom "a", Var "Y"]]
      `shouldBe`
      Just (Map.fromList [("X",Var "Y")])
    it "should perform correctly on `complex terms' example" $
      -- from Learn Prolog Now
      unify
      -- k(s(g), Y) = k(X, t(k))
      [Func "k" [Func "s" [Atom "g"], Var "Y"]]
      [Func "k" [Var "X", Func "t" [Atom "k"]]]
      `shouldBe`
      -- X = s(g), Y = t(k)
      Just (Map.fromList [("X",Func "s" [Atom "g"]),("Y",Func "t" [Atom "k"])])
    it "should reject impossible variable instantiations" $
      unify
      [Func "loves" [Var "X", Var "X"]]
      [Func "loves" [Atom "marcellus", Atom "mia"]]
      `shouldBe` Nothing
  -- hspec . describe ">>> Resolution tests" $ do
  --   it "should parse basic predicates" $ True
  --   it "should parse basic predicates" $ True
  putStrLn "Welcome to Prolog Engine 1"
  runRepl
  return ()
