-- Parser module

-----------------------------
-- Parses facts of the form:
-- (p (f (c))).
-- Where:
--  p is a predicate,
--  f is a functor and
--  c is a constant.
-- Prolog equivalent: p(f(c)).

-- Parse tree is:
-- {
--   facts = {
--     {
--       functor = "p",
--       args = {
--         {
--           functor = "f",
--           args = {
--             {
--               value = "c",
--               type = "constant"
--             }
--           },
--           type = "functor"
--         }
--       },
--       type = "fact"
--     }
--   },
--   rules = {
--   }
-- }

-----------------------------
-- Parses rules of the form:
-- (:RULE (g (X)) (p1 (Y)) (p2 (Z1) (Z2))).
-- Where:
--  g is the goal clause,
--  p1 and p2 are body predicates.
-- Prolog equivalent: g(X) :- p1(Y), p2(Z1, Z2).

-- Parse tree is:
-- {
--   facts = {
--   },
--   rules = {
--     {
--       body = {
--         {
--           functor = "p1",
--           args = {
--             {
--               value = "Y",
--               type = "variable"
--             }
--           },
--           type = "functor"
--         },
--         {
--           functor = "p2",
--           args = {
--             {
--               value = "Z1",
--               type = "variable"
--             },
--             {
--               value = "Z2",
--               type = "variable"
--             }
--           },
--           type = "functor"
--         }
--       },
--       predicate = {
--         functor = "g",
--         args = {
--           {
--             value = "X",
--             type = "variable"
--           }
--         },
--         type = "functor"
--       },
--       type = "rule"
--     }
--   }
-- }

local parser = {}

local _dump = require("pl.pretty").dump

-- Ref: http://www.inf.puc-rio.br/~roberto/lpeg/
local lpeg = require("lpeg")
local re = require("re")

local P, R, S, V = lpeg.P, lpeg.R, lpeg.S, lpeg.V
local C, Cc, Cf, Cg, Cs, Ct, Cmt = lpeg.C, lpeg.Cc, lpeg.Cf, lpeg.Cg, lpeg.Cs, lpeg.Ct, lpeg.Cmt

-- Capture handling

local parseContext = {}

local Symbol = {} -- symbol class
Symbol.__index = Symbol
function Symbol.__eq(sym1, sym2)
   return sym1.type and
	  sym1.type == sym2.type and
	  sym2.value and
	  sym1.value == sym2.value
end

local function _newSymbol(type, value)
   local sym = {type=type, value=value}
   setmetatable(sym, Symbol)
   return sym
end

local function _newConstant(value)
   return _newSymbol("constant", value)
end

local function _newVariable(value)
   return _newSymbol("variable", value)
end

local Functor = {} -- functor class
Functor.__index = Functor
function Functor.__eq(f1, f2)
   return f1.type == "functor" and
	  f1.type == f2.type and
	  f1.functor == f2.functor and
	  #f1.args == #f2.args and
	  f1:_argsEqual(f2)
end

function Functor:_argsEqual(f)
   for i, arg in ipairs(self.args) do
	  if arg ~= f.args[i] then
		 return false
	  end
   end
   return true
end

function Functor:argsContains(f)
   for i, arg in ipairs(self.args) do
	  if arg == f then
		 return true
	  end
   end
   return false
end

local function _newCompoundTerm(functor, args)
   assert(functor.type == "constant" or functor.type == "variable")
   -- don't create a compound term unless necessary
   if #args == 0 then
	  return functor
   else
	  local f = {type="functor", functor=functor.value, args=args}
	  setmetatable(f, Functor)
	  return f
   end
end

local function _addFact(functor, args)
   assert(functor.type == "constant")
   local f = {type="fact", functor=functor.value, args=args}
   table.insert(parseContext.facts, f)
end

local function _addRule(predicate, body)
   local r = {type="rule", predicate=predicate, body=body}
   table.insert(parseContext.rules, r)
end

-- Parser definition

local function sexp(contents)
   return P"(" * contents * P")"
end

local spc = S" \t\n"^0

local ruleMarker = P":RULE"

local constant = C(R"az"*(R"AZ"+R"az"+R"09")^0)/_newConstant
local variable = C(R"AZ"*(R"AZ"+R"az"+R"09")^0)/_newVariable

local function makeGrammar(elem, term)
   return P{elem;
			functor = sexp(term * spc * V"functors") / _newCompoundTerm,
			functors = Ct((V"functor" * spc)^0)
   }
end

local groundFunctor = makeGrammar("functor", constant)
local groundFunctors = makeGrammar("functors", constant)

local nongroundFunctor = makeGrammar("functor", constant + variable)
local nongroundFunctors = makeGrammar("functors", constant + variable)

local fact = sexp(constant * spc * groundFunctors) * P"." / _addFact

local rule = sexp(ruleMarker * spc * nongroundFunctor * spc * nongroundFunctors) * P"." / _addRule

local clauses = ((rule + fact) * spc)^1

function parser.parse(text)
   parseContext = {rules = {}, facts = {}}

   local ret = lpeg.match(clauses, text)

   -- for debugging
   if false then
	  print("RET:")
	  _dump(ret)
	  print("PARSE CONTEXT:")
	  _dump(parseContext)
   end

   if ret then
	  return parseContext
   end
end

return parser
