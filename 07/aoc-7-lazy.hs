-- Using a lazy map.

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy ((!))
import Data.Bits
import Data.Word

a .<. b = shiftL a (fromIntegral b)
a .>. b = shiftR a (fromIntegral b)

{------------------------------------------------------------------------------}

geta input = values ! "a"
    where
    values = Map.fromList $ map parseEval (lines input)
    get v = case reads v :: [(Word16, String)] of
                [(w,"")] -> w
                _        -> values ! v
    parseEval line = case words line of
                [a,              "->", v] -> (v, get a)
                ["NOT", a,       "->", v] -> (v, complement (get a))
                [a, "AND", b,    "->", v] -> (v, (get a) .&. (get b))
                [a, "OR",  b,    "->", v] -> (v, (get a) .|. (get b))
                [a, "LSHIFT", b, "->", v] -> (v, (get a) .<. (get b))
                [a, "RSHIFT", b, "->", v] -> (v, (get a) .>. (get b))

{------------------------------------{ IO }------------------------------------}

main = do
    rawInput <- readFile "input.txt"
    let a = geta rawInput
    print a
    print . geta $ rawInput ++ show a ++ " -> b"
