{-
  Solving the problem with some actual algorithms and data structures,
  instead of cheating and using the GHC compiler, as in aoc-07.hs
-}

import qualified Data.Map.Strict as Map
import Data.Map ((!))
import Data.Bits
import Data.Word
import Debug.Trace

isNum s = case reads s :: [(Word16, String)] of
  [(_, "")] -> True
  _         -> False

{-----------------------------------{ Data }-----------------------------------}

data Operation = CONST Var
               | NOT Var
               | AND Var Var
               | OR Var Var
               | LSHIFT Var Var
               | RSHIFT Var Var
               deriving (Show, Read, Eq)

type Var = String
type KnownValues = Map.Map String Word16
type UnknownValues = Map.Map String Operation

dependents (CONST a) = [a]
dependents (NOT a) = [a]
dependents (AND a b) = [a,b]
dependents (OR a b) = [a,b]
dependents (LSHIFT a b) = [a,b]
dependents (RSHIFT a b) = [a,b]

apply :: (Map.Map Var Word16) -> Operation -> Word16
apply m op = case op of 
        (CONST a)    -> ev a
        (NOT a)      -> complement (ev a)
        (AND a b)    -> (ev a) .&. (ev b)
        (OR a b)     -> (ev a) .|. (ev b)
        (LSHIFT a b) -> shiftL (ev a) (fromIntegral (ev b))
        (RSHIFT a b) -> shiftR (ev a) (fromIntegral (ev b))
  where ev v 
--          | trace ("ev "++v) False = undefined
          | Map.member v m = m!v
          | otherwise = read v


{---------------------------------{ Parsing }----------------------------------}

parse :: String -> (Var, Operation)
parse line = case words line of 
    [a,              "->", v] -> (v, CONST a)
    ["NOT", a,       "->", v] -> (v, NOT a)
    [a, "AND", b,    "->", v] -> (v, AND  a b)
    [a, "OR",  b,    "->", v] -> (v, OR a b)
    [a, "LSHIFT", b, "->", v] -> (v, LSHIFT a b)
    [a, "RSHIFT", b, "->", v] -> (v, RSHIFT a b)
    _                         -> error ("Unexpected pattern: " ++ line)


{---------------------------------{ Algorithm }--------------------------------}

-- "iterative solution"
-- m is a Map of names to their known numeric value
-- ops is a List of names paired with the Operation that defines them
-- for each name in ops that is not already in m, if all its dependent values
-- are known, evaluate its vumeric value and put it in m.
-- recurse until the value of "a" is known
digest :: KnownValues -> [(String, Operation)] -> KnownValues
digest m ops 
--  | trace (show m) False = undefined
  | Map.member "a" m = m
  | otherwise = digest m' ops
  where unknown s = not (Map.member s m)     :: Bool
        known s = Map.member s m || isNum s  :: Bool
        m' = foldl try_eval m ops
        try_eval n (v,op)
          | unknown v && all known (dependents op) = Map.insert v (apply m op) n
          | otherwise = n

{------------------------------------{ IO }------------------------------------}

getInstructions = do return . map parse . lines =<< readFile "input.txt"

main = do
    ops <- getInstructions
    let m = Map.empty :: Map.Map Var Word16
    let m' = digest m ops
    let a = m' ! "a"
    putStr "Part 1: "  >> print a
    
    let ops2 = [op | op <- ops,  fst op /= "b"]
    let m2 = Map.insert "b" a m
    let m2' = digest m2 ops
    let a2 = m2' ! "a"
    putStr "Part 2: " >> print a2
