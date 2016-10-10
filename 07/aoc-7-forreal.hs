{-
  Solving the problem with some actual algorithms and data structures,
  instead of cheating and using the GHC compiler, as in aoc-07.hs
-}

import qualified Data.Map.Strict as Map
import Data.Map ((!))
import Data.Bits
import Data.Word
import Control.Monad (liftM2)

bitAnd = (.&.)
bitOr =  (.|.)

isNum s = case reads s :: [(Word16, String)] of
  [(_, "")] -> True
  _         -> False

{-----------------------------------{ Data }-----------------------------------}

type Var = String
data Operation = CONST Var
               | NOT Var
               | AND Var Var
               | OR Var Var
               | LSHIFT Var Var
               | RSHIFT Var Var
               deriving (Show, Read, Eq)

type KnownValues = Map.Map String Word16
type UnknownValues = Map.Map String Operation

-- evaluate an operation in the context of the given known values. Returns a
-- Nothing if op depends on some unknown value, and a Just otherwise.
evaluate :: KnownValues -> Operation -> Maybe Word16
evaluate kn op = case op of 
        (CONST a)    -> get a
        (NOT a)      -> complement <$> get a
        (AND a b)    -> (liftM2 bitAnd) (get a) (get b)
        (OR a b)     -> (liftM2 bitOr ) (get a) (get b)
        (LSHIFT a b) -> (liftM2 shiftL) (get a) (fromIntegral <$> (get b))
        (RSHIFT a b) -> (liftM2 shiftR) (get a) (fromIntegral <$> (get b))
  where get v | isNum v   = Just (read v)
              | otherwise = Map.lookup v kn

{---------------------------------{ Parsing }----------------------------------}

parse :: String -> (Var, Operation)
parse line = case words line of 
    [a,              "->", v] -> (v, CONST a)
    ["NOT", a,       "->", v] -> (v, NOT a)
    [a, "AND", b,    "->", v] -> (v, AND  a b)
    [a, "OR",  b,    "->", v] -> (v, OR a b)
    [a, "LSHIFT", b, "->", v] -> (v, LSHIFT a b)
    [a, "RSHIFT", b, "->", v] -> (v, RSHIFT a b)

{---------------------------------{ Algorithm }--------------------------------}

-- "iterative solution"
-- fold over the entries if unknown. If the operation evaluates to
-- a Just value, insert it into known and remove it from unknown.
-- repeat the fold until all are known.
digest :: KnownValues -> UnknownValues -> KnownValues
digest known unknown 
  | Map.null unknown = known
  | otherwise = digest known' unknown'
  where
    (known', unknown') = Map.foldlWithKey evalSome (known, unknown) unknown
    evalSome (kn, un) k op = case evaluate known op of
            Just i  -> ( Map.insert k i kn
                       , Map.delete k un )
            Nothing -> (kn, un)

{------------------------------------{ IO }------------------------------------}

getInstructions :: IO UnknownValues
getInstructions = do return . Map.fromList . map parse
                    . lines =<< readFile "input.txt"

main = do
    ops <- getInstructions
    let known = digest (Map.empty) ops
    let a = known ! "a"
    putStr "Part 1: "  >> print a
    
    let known2 = digest (Map.singleton "b" a) (Map.delete "b" ops)
    putStr "Part 1: "  >> print (known2 ! "a")
