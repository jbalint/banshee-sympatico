-- basic description logic reasoner

import Prelude
import Test.Hspec
import qualified Data.Map as Map
import Data.List
import Control.Monad.Writer hiding (All)
import Data.Maybe (isJust, fromMaybe)

import GHC.Exts hiding (Any)

-- a constant represents an individual in the kb
data Constant = C String
              deriving (Show, Eq)

type ConceptName = String
type RoleName = String

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
  atomicDefs :: Map.Map ConceptName Concept,
  memberAssertions :: [Member],
  subsumptionAssertions :: [Subsumption],
  -- no atomic => composite equivs
  equivalenceAssertions :: [Equivalence]
  } deriving (Show, Eq)

ex1 :: Concept
ex1 = And [Atomic "Wine",
           Fills "Color" (C "red"),
           Exists 2 "GrapeType"]

progressiveCompanyDecl :: Equivalence
progressiveCompanyDecl = Equiv
                         (Atomic "ProgressiveCompany")
                         (And [Atomic "Company",
                               Exists 7 "Director",
                               All "Manager" (And [Atomic "Woman",
                                                   Fills "Degree" (C "phD")]),
                               Fills "MinSalary" (C "$24/hr")])

wellRoundedCoDecl :: Equivalence
wellRoundedCoDecl = Equiv
                    -- a well rounded company is:
                    (Atomic "WellRoundedCo")
                    -- a company
                    (And [Atomic "Company",
                          -- where all managers are biz school grads
                          All "Manager" (And [Atomic "B-SchoolGrad",
                                              -- and have at least 1 tech degree
                                              Exists 1 "TechnicalDegree"])])

highTechCoDecl :: Equivalence
highTechCoDecl = Equiv
                 (Atomic "HighTechCo")
                 (And [Atomic "Company",
                       Fills "Exchange" (C "nasdaq"),
                       All "Manager" (Atomic "Techie")])

techieDecl :: Equivalence
techieDecl = Equiv
             (Atomic "Techie")
             (Exists 2 "TechnicalDegree")

redBordeauxWineDecl :: Equivalence
redBordeauxWineDecl = Equiv
                      (Atomic "RedBordeauxWine")
                      (And [Atomic "Wine",
                            Fills "Color" (C "red"),
                            Fills "Region" (C "bordeaux")])

dryRedBordeauxWineDecl :: Equivalence
dryRedBordeauxWineDecl = Equiv
                         (Atomic "DryRedBordeauxWine")
                         (And [Atomic "Wine",
                               Fills "Color" (C "red"),
                               Fills "Region" (C "bordeaux"),
                               Fills "SugarContent" (C "dry")])

-- pattern match errors mean non-atomic left-hand side was passed as a param
buildAtomicEquivMap :: [Equivalence] -> Map.Map ConceptName Concept
buildAtomicEquivMap = let addToMap m (Equiv (Atomic name) complexConcept) =
                            Map.insert name complexConcept m
                      in foldl addToMap Map.empty

isEquivDecl :: Equivalence -> Bool
isEquivDecl (Equiv (Atomic _) _) = True
isEquivDecl _ = False

splitEquivDecls :: [Equivalence] -> ([Equivalence], [Equivalence])
splitEquivDecls = partition isEquivDecl

-- expand an atomic concept that's defined in terms of a complex concept
expandDefinition :: Concept -> Map.Map ConceptName Concept -> Concept
expandDefinition c@(Atomic cname) defs = fromMaybe c (Map.lookup cname defs)
expandDefinition c defs = c

-- take the contents of an AND and collapse the internal AND composites
flatten :: [Concept] -> [Concept]
flatten (And concepts:xs) = flatten concepts ++ flatten xs
flatten (All rn (And cs):xs) = All rn (And $ flatten cs) : flatten xs
flatten (x:xs) = x : flatten xs
flatten [] = []

-- we don't need to go deeper due to iterative normalization
-- flattenW :: [Concept] -> Writer Any [Concept]
-- flattenW (And concepts:xs) = tell (Any True) >> flattenW (concepts ++ xs)
-- flattenW (x:xs) = do
--   x' <- case x of (All rn (And cs)) -> All rn (And $ flattenW cs)
--                   _ -> return [x]
--   newXs <- flattenW xs
--   return (x:newXs)
-- flattenW [] = return []

flattenW :: [Concept] -> Writer Any [Concept]
flattenW (And concepts:xs) = tell (Any True) >> flattenW (concepts ++ xs)
flattenW (All rn (And cs):xs) =
  do
    cs' <- flattenW cs
    xs' <- flattenW xs
    return $ All rn (And cs'):xs'
flattenW (x:xs) = do
  xs' <- flattenW xs
  return (x:xs')
flattenW [] = return []

-- partition the alls by role, first element is non-ALL concepts
partitionAlls :: [Concept] -> [[Concept]]
-- partitionAlls = let getAllRole (All r _) = Just r
--                     getAllRole _ = Nothing
--                 in groupWith getAllRole
partitionAlls = groupWith getAllRole
                where getAllRole (All r _) = Just r
                      getAllRole _ = Nothing

partitionExists :: [Concept] -> [[Concept]]
partitionExists = groupWith getExistsRole
                  where getExistsRole (Exists _ r) = Just r
                        getExistsRole _ = Nothing

combineAllW :: [Concept] -> Writer Any [Concept]
combineAllW concepts = let (nonAlls:alls) = partitionAlls concepts
                           getConcept (All _ c) = c
                           combine [c] = return c -- single elem, no combination possible
                           combine cs@(All r _ : _) = tell (Any True) >> return (All r . And $ map getConcept cs)
                       in do { newAlls <- mapM combine alls; return $ nonAlls ++ newAlls}

combineExistsW :: [Concept] -> Writer Any [Concept]
combineExistsW concepts = let (nonExists:exists) = partitionExists concepts
                              getN :: Concept -> Int
                              getN (Exists n _) = n
                              combine [c] = return c
                              --combine cs@(Exists _ r : _) = tell (Any True) >> return (Exists (foldl (\n c -> max n $ getN c) 0 cs) r)
                              combine cs@(Exists _ r : _) = tell (Any True) >> return (Exists ((foldl . flip) (max . getN) 0 cs) r)
                          in do { newExists <- mapM combine exists; return $ nonExists ++ newExists }

normalize :: Concept -> Map.Map ConceptName Concept -> Concept
normalize c@(Atomic _) defs = And [expandDefinition c defs]
normalize (All rn c) defs = And [All rn $ normalize c defs]
normalize c@(Exists _ _) defs = And [c]
normalize c@(Fills _ _) defs = And [c]
normalize (And cs) defs =
  let normSteps = flattenW >=> combineAllW >=> combineExistsW in
  case runWriter $ normSteps cs of
   (cs', Any True) -> normalize (And $ nub cs') defs
   (cs', Any False) -> And $ nub cs'

flatten' :: [Concept] -> [Concept]
flatten' (And concepts:xs) = flatten concepts ++ flatten xs
--flatten' (All rn (And cs):xs) = All rn (And $ flatten cs) : flatten xs
flatten' (x:xs) = x : flatten xs
flatten' [] = []

combineAll :: [Concept] -> [Concept]
combineAll concepts =
  let (nonAlls:alls) = partitionAlls concepts
      getConcept (All _ c) = c
      combine [c] = c -- single elem, no combination possible
      combine cs@(All r _ : _) = All r . And $ map getConcept cs
  in nonAlls ++ map combine alls

combineExists :: [Concept] -> [Concept]
combineExists concepts =
  let (nonExists:exists) = partitionExists concepts
      getN :: Concept -> Int
      getN (Exists n _) = n
      combine [c] = c
      combine cs@(Exists _ r : _) = Exists ((foldl . flip) (max . getN) 0 cs) r
  in nonExists ++ map combine exists

normalize' :: Concept -> Map.Map ConceptName Concept -> Concept
normalize' c@(Atomic _) defs = expandDefinition c defs
normalize' (All rn c) defs = All rn $ normalize' c defs
normalize' c@(Exists _ _) defs = c
normalize' c@(Fills _ _) defs = c
normalize' (And cs) defs =
  let normCs = map (`normalize'` defs) cs
      flatCs = flatten' normCs
      allCombCs = combineAll flatCs
      existsCombCs = combineExists allCombCs in
  And $ nub $ map (flip normalize' defs) existsCombCs

-- getN2 :: Concept -> Int
-- getN2 (Exists n _) = n

-- TODO fix Thing

-- new way that findSubsumer can be rewritten
newFindSub :: Concept -> [Concept] -> Maybe Concept
newFindSub e =
  let pred = case e of
              Atomic _ -> (== e)
              Exists 1 r -> (\x -> case x of Fills r2 _ | r == r2 -> True)
  in find pred

-- e = subsumer
-- d = subsumed
findSubsumer :: Concept -> [Concept] -> Maybe Concept
findSubsumer e@(Atomic _) d = find (== e) d
findSubsumer e@(Fills _ _) d = find (== e) d
findSubsumer (Exists 1 r) d = find (\x -> case x of Fills r2 _ | r == r2 -> True
                                                    Exists n r2 | r == r2 && n >= 1 -> True
                                                    _ -> False) d
findSubsumer (Exists n r) d = find (\x -> case x of Exists n2 r2 | r == r2 && n2 >= n -> True
                                                    _ -> False) d
findSubsumer (All rn c) d = find (\x -> case x of All rn2 c2 | rn == rn2 && isJust (findSubsumer c [c2]) -> True
                                                  _ -> False) d
findSubsumer (And cs) d = find (\x -> case x of
                                       And d_cs -> map (flip findSubsumer $ ) cs

-- what I need:
-- for EACH concept in the And construct of e:
--    find a single AND in d where:
--        


subsumedBy :: Concept -> Concept -> Bool
subsumedBy (And cs1) (And cs2) = (foldl .flip) ((&&) . isJust) True $ map (`findSubsumer` cs2) cs1

testKbDef :: KbDef
testKbDef = KbDef
            []
            [progressiveCompanyDecl, wellRoundedCoDecl, highTechCoDecl, techieDecl,
             redBordeauxWineDecl, dryRedBordeauxWineDecl]
            [Member (C "joe") (Atomic "Person"),
             Member (C "canCorp") (And [Atomic "Company",
                                        All "Manager" (Atomic "Canadian"),
                                        Fills "Manager" (C "joe")]),
             Member (C "ellen") (And [Exists 1 "Child",
                                      All "Child" (And [Fills "Pediatrician" (C "marianne"),
                                                        All "Pediatrician" (Atomic "Scandinavian")])])]

parseKbDef :: KbDef -> Kb
parseKbDef (KbDef subs equivs members) =
  let (atomics, nonAtomics) = splitEquivDecls equivs
      atomicDefs2 = buildAtomicEquivMap atomics
  in
   Kb {
     atomicDefs = atomicDefs2,
     memberAssertions = members,
     subsumptionAssertions = [],
     -- todo is this necessary? should be able to keep all equivs as definitions
     -- rewrite the equiv structure to handle this: Def ConceptName [non-atomic-]Concept
     equivalenceAssertions = nonAtomics
     }

testKb :: Kb
testKb = parseKbDef testKbDef

testSubsumption1 :: Expectation
testSubsumption1 =
  let d = And [Atomic "Company",
               All "Manager" (Atomic "B-SchoolGrad"),
               Exists 1 "Exchange"]
      d' = case (wellRoundedCoDecl, highTechCoDecl) of (Equiv _ a, Equiv _ b) -> normalize' (And [a,b]) $ atomicDefs testKb
  in d' `subsumedBy` d `shouldBe` True

main :: IO ()
main = do
  print testKb
  print ex1
  putStrLn "Ok"
  hspec . describe "flatten" $
    it "should nest properly" $
    flatten [And [And [Atomic "a", Atomic "b"], All "Manager" (And [And [Atomic "a", Atomic "b"]])]] `shouldBe` [Atomic "a",Atomic "b",All "Manager" (And [Atomic "a",Atomic "b"])]
  return ()
