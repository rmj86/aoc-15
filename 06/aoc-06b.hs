-- solving aoc-06 with a different algorithm. Iterating over the commands
-- for each point rather than iterating over the points for each command
-- Should be faster, at least for part 1, because all previous work is nulled
-- at a TurnOn/Off command, meaning you can ignore all previous instructions
-- It's a bit faster for part 1, but significantly slower for part 2 :(

import Data.Array.Unboxed (Array, listArray, accum, elems)
import Data.List (foldl')

{-------------------------------  Part The 1st  -------------------------------}

type Point = (Int, Int)
type BBox = (Int, Int, Int, Int)  -- [xmin,ymin,xmax,ymax]
data CommandType = TurnOn | TurnOff | Toggle
                 deriving (Show)
type Command = (CommandType, BBox)

points = [(i,j) | i <- [0..999], j <- [0..999]]

contains (x0,y0,x1,y1) (i,j) = and [ x0 <= i
                                   , i <= x1
                                   , y0 <= j
                                   , j <= y1]

evalPoint1 :: Point -> [Command] -> Bool
evalPoint1 _ [] = False
evalPoint1 p ((c,bb):cs) = case bb `contains` p of
        False -> evalPoint1 p cs
        True -> case c of
            TurnOn -> True
            TurnOff -> False
            Toggle -> not $ evalPoint1 p cs

solution1 cs = length . filter id $ [evalPoint1 p cs | p<-points]

{-------------------------------  Part The 2nd  -------------------------------}

evalPoint2 :: Point -> [Command] -> Int
evalPoint2 _ [] = 0
evalPoint2 p ((c,bb):cs) = case bb `contains` p of
        False -> n
        True -> case c of
            TurnOn -> n+1
            TurnOff -> max 0 $ n-1
            Toggle -> n+2
  where n = evalPoint2 p cs

solution2 cs = sum $ [evalPoint2 p cs | p<-points]

-- IO --

parse_bbox ws = read $ "(" ++ (p1 ++ "," ++ p2) ++ ")"
  where [p1,_,p2] = ws

parse :: String -> Command
parse s = case words s of
        "toggle" : ps -> (Toggle, parse_bbox ps)
        "turn"   : ws -> case ws of
            "on"  : ps -> (TurnOn,  parse_bbox ps)
            "off" : ps -> (TurnOff, parse_bbox ps)

{------------------------------------  IO  ------------------------------------}

getData = return . reverse . map parse . lines =<< readFile "input.txt"

main = do cs <- getData
          print $ solution1 cs
          print $ solution2 cs
