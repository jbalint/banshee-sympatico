-- v2 (cleanup) of DL reasoner

import Prelude
import Test.Hspec
import qualified Data.Map as Map
import Data.List (nub, find)
import Data.Maybe (fromMaybe, isJust)

import GHC.Exts hiding (Any)

-- a constant represents an individual in the kb
data Constant = C String
              deriving (Show, Eq)

type ConceptName = String
type RoleName = String
-- map of atomic concept definitions
type AtomicDefMap = Map.Map ConceptName Concept

data Concept = Atomic ConceptName
             | All RoleName Concept
             | Exists Int RoleName
             | Fills RoleName Constant
             | And [Concept]
             deriving (Show, Eq)

data Role = Role Constant Constant
          deriving (Show, Eq)

data Subsumption = Sub Concept Concept
                 deriving (Show, Eq)

data Equivalence = Equiv Concept Concept
                 deriving (Show, Eq)

data Member = Member Constant Concept
            deriving (Show, Eq)

-- KB definition, easier to write
data KbDef = KbDef [Subsumption] [Equivalence] [Member]

-- KB type used in computations, easier to use
data Kb = Kb {
  atomicDefs :: AtomicDefMap
  ,memberAssertions :: [Member]
  ,subsumptionAssertions :: [Subsumption]
  } deriving (Show, Eq)

-- ============= --
-- normalization --
-- ============= --
flatten :: [Concept] -> [Concept]
flatten (And concepts:xs) = concepts ++ flatten xs
flatten (x:xs) = x : flatten xs
flatten [] = []

combineAlls :: [Concept] -> AtomicDefMap -> [Concept]
combineAlls cs atomics =
  -- TODO how to simplify this lambda?
  let (noCombine:toCombine) = groupWith (\x -> case x of (All r _) -> Just r
                                                         _ -> Nothing) cs
      getConcept (All _ c) = normalize c atomics
      combine [c] = normalize c atomics
      combine cs2@(All r _:_) = All r . (`normalize` atomics) $ And (map getConcept cs2)
  in noCombine ++ map combine toCombine

combineExists :: [Concept] -> [Concept]
combineExists cs =
  -- TODO use standard groupBy
  let groups = groupWith (\x -> case x of (Exists _ r) -> Just r
                                          _ -> Nothing) cs
      (noCombine, toCombine) = break (\x -> case x of (Exists _ _:_) -> True
                                                      _ -> False) groups
      getMinRel (Exists n _) = n
      combine [c] = c
      combine cs2@(Exists _ rn:_) = let maxN = foldl (flip $ max . getMinRel) 0 cs2
                                       in Exists maxN rn
  in concat noCombine ++ map combine toCombine

normalize :: Concept -> AtomicDefMap -> Concept
normalize c@(Atomic cname) atomics = fromMaybe c (Map.lookup cname atomics)
normalize (All rn innerC) atomics = All rn (normalize innerC atomics)
normalize (And components) atomics =
  let cs = map (`normalize` atomics) components
      cs' = flatten cs
      cs'' = combineAlls cs' atomics
      cs''' = combineExists cs''
  in case length cs''' of 1 -> head cs'''
                          _ -> And $ nub cs'''
normalize c _ = c -- covers exists and fills

normalize2 :: Concept -> AtomicDefMap -> Concept
normalize2 c@(And _) defs = normalize c defs
normalize2 c defs = And [normalize c defs]

normalizationTests :: Spec
normalizationTests =
  let atomics = Map.fromList [("SomeExists", Exists 2 "Something"),
                              ("Jess", Exists 1 "AJess")]
  in describe ">>> normalization tests" $ do
    -- trivial test of replacing an atomic with it's definition
    it "should resolve simple atomics" $
      normalize (Atomic "SomeExists") atomics `shouldBe` Exists 2 "Something"
    -- test `normalize' handling of `All'
    it "should normalize ALL's component" $
      normalize (All "xyz" (Atomic "SomeExists")) atomics `shouldBe`
      All "xyz" (Exists 2 "Something")
    -- test of `combineAlls'
    it "should combine ALLs" $
      combineAlls [All "a" $ Atomic "Jess",
                   Atomic "Separator",
                   All "b" $ Atomic "SomethingElseEntirely",
                   All "a" $ And [Atomic "Michael", Atomic "Balint"]] atomics
      `shouldBe` [Atomic "Separator",
                  All "a" (And [Atomic "Michael",
                                Atomic "Balint",
                                Exists 1 "AJess"]),
                  All "b" (Atomic "SomethingElseEntirely")]
    -- test of `combineExists'
    it "should combine EXISTSs" $
      combineExists [Exists 2 "Monkey",
                     Atomic "Separator",
                     Exists 8 "Fireball",
                     Exists 3 "Monkey"]
      `shouldBe` [Atomic "Separator",Exists 8 "Fireball",Exists 3 "Monkey"]
    -- same as previous through `normalize'
    it "should combine EXISTSs with `normalize'" $
      normalize (And [Exists 2 "Monkey",
                      Atomic "Separator",
                      Exists 8 "Fireball",
                      Exists 3 "Monkey"]) atomics
      `shouldBe` And [Atomic "Separator",Exists 8 "Fireball",Exists 3 "Monkey"]
    -- XXXX
    it "should normalize a complex structure" $
      let complexConcept =
            All "a" $ And [Exists 2 "AJess", Atomic "Jess"]
      in normalize complexConcept atomics
         `shouldBe` All "a" (Exists 2 "AJess")
    it "should normalize an example" $
      let atomics' = Map.fromList
                     [("WellRoundedCo", And [Atomic "Company",
                                             -- where all managers are biz school grads
                                             All "Manager" (And [Atomic "B-SchoolGrad",
                                                                 -- and have at least 1 tech degree
                                                                 Exists 1 "TechnicalDegree"])]),
                      ("HighTechCo", And [Atomic "Company",
                                          Fills "Exchange" (C "nasdaq"),
                                          All "Manager" (Atomic "Techie")]),
                      ("Techie", Exists 2 "TechnicalDegree")]
          exampleConcept = And [Atomic "WellRoundedCo", Atomic "HighTechCo"]
      in normalize exampleConcept atomics'
         `shouldBe` And [Atomic "Company",Fills "Exchange" (C "nasdaq"),All "Manager" (And [Atomic "B-SchoolGrad",Exists 2 "TechnicalDegree"])]

-- =========== --
-- subsumption --
-- =========== --
-- check by structure matching
-- e = subsuming concept
-- d = set to search for subsumed concept
findSubsumed :: Concept -> [Concept] -> Maybe Concept
findSubsumed e@(Atomic cn) d = find (\x -> case x of Atomic cn2 | cn == cn2 -> True
                                                     And cs -> isJust $ findSubsumed e cs
                                                     _ -> False) d
findSubsumed e@(Fills rn c) d = find (\x -> case x of Fills rn2 c2 | rn == rn2 && c == c2 -> True
                                                      And cs -> isJust $ findSubsumed e cs
                                                      _ -> False) d
findSubsumed e@(Exists 1 r) d = find (\x -> case x of Fills r2 _ | r == r2 -> True
                                                      Exists n r2 | r == r2 && n >= 1 -> True
                                                      And cs -> isJust $ findSubsumed e cs
                                                      _ -> False) d
findSubsumed e@(Exists n r) d = find (\x -> case x of Exists n2 r2 | r == r2 && n2 >= n -> True
                                                      And cs -> isJust $ findSubsumed e cs
                                                      _ -> False) d
findSubsumed e@(All rn c) d = find (\x -> case x of All rn2 c2 | rn == rn2 && isJust (findSubsumed c [c2]) -> True
                                                    And cs -> isJust $ findSubsumed e cs
                                                    _ -> False) d
-- given: AND_1 = {c_1, ..., c_i}, AND_2 = {d_1, ..., d_j}
-- AND_1 \sqsubseteq AND_2 iff \forall d_n \in AND_2 \exists c_m \in AND_1 s.t. c_m \sqsubseteq d_n
findSubsumed (And cs) d =
  let dAnds = filter (\x -> case x of And _ -> True
                                      _ -> False) d
  in find (\(And d_n) -> all isJust $ map (`findSubsumed` d_n) cs) dAnds

subsumedBy :: Concept -> Concept -> Bool
subsumedBy (And subsumedCs) (And subsumerCs) = (foldl . flip) ((&&) . isJust) True $ map (`findSubsumed` subsumedCs) subsumerCs

subsumptionTests :: Spec
subsumptionTests =
  describe ">>> subsumption tests" $ do
    it "should find a subsumed FILLS for EXISTS 1" $
      let subsumed = findSubsumed (Exists 1 "SomeRole") [Fills "SomeRole" $ C "X"]
      in subsumed `shouldBe` Just (Fills "SomeRole" $ C "X")
    it "should find a subsumed EXIST 2 for EXISTS 1" $
      let subsumed = findSubsumed (Exists 1 "SomeRole") [Exists 2 "SomeRole"]
      in subsumed `shouldBe` Just (Exists 2 "SomeRole")
    it "should find NOT a subsumed EXIST 1 for EXISTS 2" $
      let subsumed = findSubsumed (Exists 2 "SomeRole") [Exists 1 "SomeRole"]
      in subsumed `shouldBe` Nothing
    it "should find subsumed recursively for ALL (simple)" $
      let subsumed = findSubsumed (All "People" $ Atomic "HaveLegs") [All "People" $ Atomic "HaveLegs"]
      in subsumed `shouldBe` Just (All "People" $ Atomic "HaveLegs")
    it "should find subsumed for basic AND case" $
      -- let subsumed = And [Atomic "B", Exists 1 "SomeRole", Atomic "C"]
      --     subsumer = And [Exists 2 "SomeRole", Atomic "B"]
      let subsumed = And [Atomic "B", Exists 3 "SomeRole", Atomic "C"]
          subsumer = And [Atomic "B", Exists 2 "SomeRole"]
      in (findSubsumed subsumer [subsumed], subsumedBy subsumed subsumer)
         `shouldBe` (Just (And [Atomic "B",Exists 3 "SomeRole", Atomic "C"]), True)
    it "should work with an example" $
      let subsumer = And [All "Manager" (Atomic "B-SchoolGrad"),
                          Exists 1 "Exchange"]
          subsumed = And [Atomic "Company",
                          All "Manager" (And [Atomic "B-SchoolGrad",
                                              Exists 2 "TechnicalDegree"]),
                          Fills "Exchange" $ C "nasdaq"]
      in subsumedBy subsumed subsumer `shouldBe` True
    it "should find specific `All' concepts inside AND" $
      findSubsumed (All "m" (Atomic "n")) [And [Atomic "A",
                                                Exists 1 "x",
                                                All "m" (Atomic "n")]]
      `shouldBe` Just (And [Atomic "A",Exists 1 "x",All "m" (Atomic "n")])
    it "should find specific `Exists' concepts inside AND" $
      let subsumed = And [Atomic "A", Exists 3 "x"]
      in findSubsumed (Exists 1 "x") [subsumed] `shouldBe` Just subsumed

main :: IO ()
main = do
  putStrLn "hey"
  hspec normalizationTests
  hspec subsumptionTests
  return ()

