local parser = require("parser")

local _dump = require("pl.pretty").dump

describe("parser", function ()
			context("very basics", function ()
					   it("should parse a fact", function ()
							 parser.parse("(p (a) (b (c))).")
					   end)
					   it("should parse a rule", function ()
							 parser.parse("(:RULE (p (X)) (X)).")
					   end)
					   it("should parse the examples", function ()
							 local x = parser.parse("(p (f (c))).")
							 --_dump(x)
							 local y = parser.parse("(:RULE (g (X)) (p1 (Y)) (p2 (Z1) (Z2))).")
							 --_dump(y)
					   end)
			end)

			context("fact parsing", function ()
					   it("should parse a zero-argument constant fact", function ()
							 local parse = parser.parse("(x).")
							 assert_equal(0, #parse.rules)
							 assert_equal(1, #parse.facts)
							 local f = parse.facts[1]
							 assert_equal("x", f.functor)
							 assert_equal(0, #f.args)
					   end)
					   it("should not parse a zero-argument variable fact", function ()
							 local parse = parser.parse("(X).")
							 assert_equal(nil, parse)
					   end)
					   it("should parse facts with 1 arguments", function ()
							 local parse = parser.parse("(x (y)).")
							 assert_equal(0, #parse.rules)
							 assert_equal(1, #parse.facts)
							 local f = parse.facts[1]
							 assert_equal("x", f.functor)
							 assert_equal(1, #f.args)
							 f = f.args[1]
							 assert_equal("constant", f.type)
							 assert_equal("y", f.value)
					   end)
					   it("should parse facts with 2 arguments", function ()
							 local parse = parser.parse("(x (y) (z)).")
							 assert_equal(0, #parse.rules)
							 assert_equal(1, #parse.facts)
							 local f = parse.facts[1]
							 assert_equal("x", f.functor)
							 assert_equal(2, #f.args)
							 f = parse.facts[1].args[1]
							 assert_equal("constant", f.type)
							 assert_equal("y", f.value)
							 f = parse.facts[1].args[2]
							 assert_equal("constant", f.type)
							 assert_equal("z", f.value)
					   end)
					   it("should parse facts with 1 nested arguments", function ()
							 local parse = parser.parse("(x (y (z))).")
							 assert_equal(0, #parse.rules)
							 assert_equal(1, #parse.facts)
							 local f = parse.facts[1]
							 assert_equal("x", f.functor)
							 assert_equal(1, #f.args)
							 f = f.args[1]
							 assert_equal("functor", f.type)
							 assert_equal("y", f.functor)
							 assert_equal(1, #f.args)
							 f = f.args[1]
							 assert_equal("constant", f.type)
							 assert_equal("z", f.value)
					   end)
			end)

			context("rule parsing", function ()
					   it("should NOT parse a rule with no body", function ()
							 -- TODO
					   end)
					   it("should NOT parse a rule with a variable as a predicate symbol", function ()
							 -- TODO
					   end)
					   it("should parse a rule with a zero-argument head", function ()
							 local parse = parser.parse("(:RULE (q) (p)).")
							 assert_equal(1, #parse.rules)
							 assert_equal(0, #parse.facts)
							 local r = parse.rules[1]
							 local pred = r.predicate
							 assert_equal("constant", r.predicate.type)
							 assert_equal("q", r.predicate.value)
							 assert_equal(1, #r.body)
							 assert_equal("constant", r.body[1].type)
							 assert_equal("p", r.body[1].value)
					   end)
					   it("should parse a rule with a one-variable-argument head", function ()
							 local parse = parser.parse("(:RULE (q (X)) (p (X))).")
							 assert_equal(1, #parse.rules)
							 assert_equal(0, #parse.facts)
							 local r = parse.rules[1]
							 local pred = r.predicate
							 assert_equal("functor", pred.type)
							 assert_equal("q", pred.functor)
							 assert_equal(1, #pred.args)
							 assert_equal("variable", pred.args[1].type)
							 assert_equal("X", pred.args[1].value)
							 assert_equal(1, #r.body)
							 local f = r.body[1]
							 assert_equal("functor", f.type)
							 assert_equal("p", f.functor)
							 assert_equal(1, #f.args)
							 assert_equal("variable", f.args[1].type)
							 assert_equal("X", f.args[1].value)
					   end)
			end)

			context("parse tree methods", function ()
					   it("should compare two separate constants as the same", function ()
							 local a = parser.parse("(d (x)).")
							 local b = parser.parse("(c (x)).")
							 assert_equal(a.facts[1].args[1], b.facts[1].args[1])
					   end)
					   it("should compare two separate variables as the same", function ()
							 local a = parser.parse("(:RULE (q (A)) (p)).")
							 local b = parser.parse("(:RULE (a (B) (A)) (b)).")
							 assert_equal(a.rules[1].predicate.args[1],
										  b.rules[1].predicate.args[2])
					   end)
					   it("should comparse two separate functors as the same", function ()
							 local a = parser.parse("(d (x (z))).")
							 local b = parser.parse("(c (x (z))).")
							 assert_equal(a.facts[1].args[1], b.facts[1].args[1])
					   end)
			end)

			context("mixed rules and facts", function ()
					   -- TODO
			end)
end)