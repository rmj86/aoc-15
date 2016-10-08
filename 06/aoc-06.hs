{-------------------------------  Part The 1st  -------------------------------}

import Data.List (foldl')

type Point = (Int, Int)
type Grid = [[Int]]

lights :: Grid
lights = [[0 | y <- [0..999]]
             | x <- [0..999]]

mapRegion f (x0,y0) (x1,y1) rows = zipWith u [0..] rows
  where u i row = if i < x0  ||  x1 < i
                  then row
                  else zipWith v [0..] row
        v j e   = if j < y0  ||  y1 < j
                  then e
                  else f e

apply :: Grid -> (String, Point, Point) -> Grid
apply ls (c, p0, p1) = case c of
        "toggle"  -> mapRegion (1-)      p0 p1 ls
        "turnon"  -> mapRegion (const 1) p0 p1 ls
        "turnoff" -> mapRegion (const 0) p0 p1 ls

solution1 cs = sum . map sum . foldl' apply lights $ cs

{-------------------------------  Part The 2nd  -------------------------------}

apply2 :: Grid -> (String, Point, Point) -> Grid
apply2 ls (c, p0, p1) = case c of
        "toggle"  -> mapRegion (+2)               p0 p1 ls
        "turnon"  -> mapRegion (+1)               p0 p1 ls
        "turnoff" -> mapRegion (\v-> max 0 (v-1)) p0 p1 ls

solution2 cs = sum . map sum . foldl' apply2 lights $ cs

{------------------------------------  IO  ------------------------------------}

parse_p :: String -> Point
parse_p s = read $ "("++s++")"

parse :: String -> (String, Point, Point)
parse s
 | w!!0=="turn" = (w!!0++w!!1, parse_p (w!!2), parse_p (w!!4))
 | w!!0=="toggle" = (w!!0, parse_p (w!!1), parse_p (w!!3))
  where w = words s

getData = do s <- readFile "input.txt"
             return $ map parse (lines s)

main = do cs <- getData
          print $ solution1 cs
          print $ solution2 cs
