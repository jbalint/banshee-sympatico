{-# LANGUAGE OverloadedStrings #-}
import System.IO

import qualified Data.Map as Map

import Control.Monad (unless)

import Data.SCargot.Basic
import Data.SCargot.General
import Data.SCargot.Repr
import qualified Data.Text as T

import Objectspace
import Syntax

import Control.Applicative ((<*), (*>), (<*>), (<$>))
import qualified Data.ByteString.UTF8 as B
import Data.Char (isAlpha, isDigit, isAlphaNum)
import Data.Maybe (fromJust)
import           Text.Parsec ( (<|>)
                             , (<?>)
                             , char, optionMaybe
                             , many1
                             , skipMany, digit , oneOf, noneOf, many, letter, alphaNum
                                                                              , parse
                             )
import           Text.Parsec.Text (Parser)

import Data.List (intercalate)

-- must have trailing slash, see uses
basePath = "/home/jbalint/sw/banshee-sympatico/data/"

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

parseString :: Parser B.ByteString
parseString = do
    char '"'
    strings <- many character
    char '"'
    return . B.fromString $ concat strings

parseId :: Parser (NsKey, ObjKey)
parseId  = do
  first <- (:) <$> letter <*> many alphaNum
  second <- optionMaybe (char ':' *> ((:) <$> letter <*> many alphaNum))
  return $ case second of
            Nothing -> (Nothing, first)
            _ -> (Just first, fromJust second)

pAtom :: Parser Term
pAtom = (TInt . read) <$> many1 digit
        <|> TString <$> parseString
        <|> TId . uncurry Id <$> parseId

sAtom :: Term -> String
-- TODO: escaping
sAtom (TString str) = "\"" ++ B.toString str ++ "\""
sAtom (TId (Id Nothing x)) = x
sAtom (TId (Id (Just ns) k)) = ns ++ ":" ++ k
sAtom (TInt int) = show int

mySpec :: SExprSpec Term (RichSExpr Term)
mySpec = (asRich (mkSpec pAtom (T.pack . sAtom)))

addField :: Fields -> RichSExpr Term -> Fields
addField f sexpr =
  let (key, value) = case sexpr of RSDotted [RSAtom (TId key)] value -> (key, value)
  in Map.insert key value f

richSexprToObj :: RichSExpr Term -> Obj
richSexprToObj sexpr =
  let exprs = case sexpr of RSList x -> x
      objKey = case head exprs of RSAtom (TId id) -> id
      fields = foldl addField Map.empty (tail exprs)
  in
  Obj objKey fields

fieldToSexpr :: PropKey -> Term -> RichSExpr Term
fieldToSexpr k v = RSDotted [RSAtom (TId k)] v

objToRichSexpr :: Obj -> RichSExpr Term
objToRichSexpr (Obj id@(Id ns k) fields) =
  let idAtom = RSAtom (TId id)
      fieldToSexpr :: PropKey -> Term -> RichSExpr Term
      fieldToSexpr k v = RSDotted [RSAtom (TId k)] v
  in RSList (idAtom : (Map.foldWithKey (\k v l -> fieldToSexpr k v : l ) [] fields))

test1 = decode mySpec $ "(test:jess (rdf:label . \"Jess\"))"

test2 = decode mySpec . T.pack $ unlines ["(rdf:Alt",
                                          "(rdf:type . rdfs:Class)",
                                          "(rdfs:comment . \"The class of containers of alternatives.\")",
                                          "(rdfs:isDefinedBy . \"<http://www.w3.org/1999/02/22-rdf-syntax-ns#>\")",
                                          "(rdfs:label . \"Alt\")",
                                          "(rdfs:subClassOf . rdfs:Container)",
                                          "(test:someNumber . 42))"
                                         ]

test1Obj = case fmap (richSexprToObj . head) test1 of Right x -> x
test2Obj = case fmap (richSexprToObj . head) test2 of Right x -> x

test1Encoded = encode mySpec [(objToRichSexpr test1Obj)]
test2Encoded = encode mySpec [(objToRichSexpr test2Obj)]

-- ========================== --
-- repl stuff copied from:
-- ========================== --
-- http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO ()
evalString command =
  let
    stringToId :: String -> Id
    stringToId str = case parse parseId "" (T.pack str) of Right x -> uncurry Id x
  in
  case command of
  -- TODO parse ID and read the object (still need lisp serialization of it)
   "read" -> do
     idStr <- getLine
     id <- return $ stringToId idStr
     ns <- return $ case id of Id (Just ns) _ -> ns
     obj <- readObject (basePath ++ ns ++ ".ospace") id
     case obj of Just obj -> putStrLn . T.unpack $ encode mySpec [objToRichSexpr obj]
                 Nothing -> putStrLn ("NOTHING: " ++ show id)
   "write" -> do
     objStr <- getLine
     obj <- return . richSexprToObj $ (case decode mySpec (T.pack objStr) of Right x -> head x)
     ns <- return $ case obj of Obj (Id (Just ns) _) _ -> ns
     writeObject (basePath ++ ns ++ ".ospace") obj
   "" -> error "Quitting"
   _ -> putStr "ERROR: unrecognized command"

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   unless (pred result) $ action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "> ") evalString

-- ===============================

main :: IO ()
main = do
  putStr $ unlines
    ["******* ** ************** *** ***** ************ ******** ******* *** *** ***** *******",
     "Welcome to `ospace_lisp', the first experimental language support for the Meera system.",
     "******* ** ************** *** ***** ************ ******** ******* *** *** ***** *******",
     "Objects can be read or written by sending the text \"read\" or \"write\" on a line by",
     "itself followed by the argument to the command. \"read\" takes a single object id argument",
     "in the form ns:id and will read the object from the objectspace named \"ns\". \"write\"",
     "takes a single argument which is the s-expr form of the object.",
     "",
     "Enter \"quit\" to exit.",
     "--------------",
     "-- Some examples:",
     "-- > read",
     "-- test:jess",
     "-- (test:jess (rdf:label . \"Jess\"))",
     "-- > write",
     "-- (rdf:Alt (rdf:type . rdfs:Class) (rdfs:comment . \"The class of ...\") (rdfs:isDefinedBy . \"<http://www.w3.org/1999/02/22-rdf-syntax-ns#>\") (rdfs:label . \"Alt\") (rdfs:subClassOf . rdfs:Container))",
     "-- >",
     "--------------",
     "Enjoy!", "", ""
    ]
  runRepl
