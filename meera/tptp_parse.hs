import Test.Hspec

import Text.ParserCombinators.Parsec

import Control.Applicative hiding ((<|>))
import Control.Monad

-- cnf(NAME, ROLE, FORMULA ANNOTATIONS).

-- FORMULA:
--   (DISJ) | DISJ
-- DISJ:
--   LITERAL | DISJ VLINE LITERAL
-- LITERAL:
--   ATOMIC_FORMULA | ~ ATOMIC_FORMULA | FOL_INFIX_UNARY
-- FOL_INFIX_UNARY:
--   TERM INFIX_INEQUALITY TERM
-- TERM:
--   FUNCTION_TERM | VARIABLE | CONDITIONAL_TERM | LET_TERM
-- FUNCTION_TERM:

-- PLAIN_TERM:
--   CONSTANT | FUNCTOR ( ARGUMENTS )
-- CONSTANT:
--   FUNCTOR
-- FUNCTOR:
--   ATOMIC_WORD
-- ATOMIC_WORD:
--   LOWER_WORD | SINGLE_QUOTED
-- ARGUMENTS:
--   TERM | TERM , ARGUMENTS
-- VARIABLE:
--   UPPER_WORD
-- CONDITIONAL_TERM: (TFF-only)


-- v 0.1 - propositional, no functions

data Term = PropVar String | NegTerm Term deriving (Eq, Show)

type Disjunction = [Term]
type CnfFormula = [Disjunction]

lowerWord :: Parser Term
lowerWord = liftM PropVar $ many1 lower

negatedWord :: Parser Term
negatedWord = char '~' >> liftM NegTerm lowerWord

term :: Parser Term
term = negatedWord <|> lowerWord

disjunction :: Parser Disjunction
-- TODO how to rewrite this
--disjunction = (<|>) (term >>= (\x -> return [x])) (term >>= (\x -> return [x]))
--disjunction = choice [term >>= (\x -> return [x]), ...]
disjunction = sepBy term (char '|')

--parseCnf :: Parser 
--parseCnf = do

-- a CNF formula, a list of disjunctions
parsesTo :: CnfFormula
parsesTo = [[PropVar "p"], [PropVar "q", NegTerm $ PropVar "p"]]

parseLowerWord :: String -> Either ParseError Term
parseLowerWord = parse (lowerWord <* eof) "myParser"

parseDisjunction :: String -> Either ParseError Disjunction
parseDisjunction = parse (disjunction <* eof) "myParser"

cnf :: Parser Disjunction
cnf = do
  string "cnf("
  lowerWord
  char ','
  lowerWord
  char ','
  x <- disjunction
  string ")."
  spaces
  return x

parseCnf :: String -> Either ParseError CnfFormula
parseCnf = parse (many1 cnf <* eof) "myParser"

theory :: CnfFormula
theory = case parseCnf "cnf(a,b,p).cnf(a,b,q|~p)." of
  Right cnf -> cnf
  Left _ -> []


containsNegTerm :: Disjunction ->  Term -> Bool
containsNegTerm [] _ = False
containsNegTerm (x:xs) (NegTerm pv) = pv == x
containsNegTerm ((NegTerm x):xs) t = x == t
containsNegTerm (_:xs) t = containsNegTerm xs t
-- containsNegTerm (x:xs) t = case x of
--   NegTerm t' | t' == t -> True
--   _ -> containsNegTerm xs t
--  _ -> t == NegTerm x || containsNegTerm xs t

-- containsNegTerm (x:xs) t | x == NegTerm t = True
--                          | t == NegTerm x = True -- if t is already a NegTerm, x is positive
--                          | True = containsNegTerm xs t


-- resolve TERM against the DISJUNCTION returning Maybe [Term] of the
-- terms that are not the negation of the first term or NOTHING otherwise

-- not a very efficient implementation because the guard will check 
-- the list first
resolveAgainst :: Term -> Disjunction -> Maybe [Term]
resolveAgainst _ [] = Nothing
resolveAgainst t dis | not $ containsNegTerm dis t = Nothing
resolveAgainst t dis = let negTerm t t' = t == NegTerm t' || t' == NegTerm t in
  Just $ filter (not . negTerm t) dis

-- whether term is true in the theory
-- query should be in negated form
resolution :: Term -> CnfFormula -> Bool
resolution query theory = False

asd :: String
asd = "1"

main :: IO ()
main = do
  print theory
  print asd
  print $ resolution (PropVar "t") []
  putStrLn "This program does nothing"
  hspec . describe "first hspec test" $
    it "should do something" True
  hspec . describe "resolveAgainst" .
    it "should make a direct resolution" $
    resolveAgainst 
  return ()
