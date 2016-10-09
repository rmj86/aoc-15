import qualified Data.Map as Map
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

digest m ops 
--  | trace (show m) False = undefined
  | Map.member "a" m = m
  | otherwise = digest m' ops
  where unknown s = not (Map.member s m)
        known s = Map.member s m || isNum s
        m' = foldl nxt m ops
        nxt n (v,op)
          | unknown v && all known (dependents op) = Map.insert v (apply m op) n
          | otherwise = n


{------------------------------------{ IO }------------------------------------}

getData = do s <- readFile "aoc-7.txt"
             return $ map parse (lines s)
getData2 = do s <- readFile "aoc-7-2.txt"
              return $ map parse (lines s)

main1 = do ops <- getData
           let m = Map.empty :: Map.Map Var Word16
           let m' = digest m ops
           putStr "Part 1: "
           print $ "a = " ++ show (m'!"a")

main2 = do ops <- getData2
           let m = Map.empty :: Map.Map Var Word16
           let m' = digest m ops
           putStr "Part 2: "
           print $ "a = " ++ show (m'!"a")


main = do main1
          main2