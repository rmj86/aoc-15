import Data.List (foldl')

{-------------------------------  Part The 1st  -------------------------------}

type Point = (Int, Int)

lights :: [[Bool]]
lights = [[False | y<-[0..999]]
                 | x<-[0..999]]

applyInRegion u (x0,y0) (x1,y1) ls = zipWith f [0..] ls
  where
    f i xs =
      if i<x0 then xs else
        if i>x1 then xs else
          zipWith g [0..] xs
    g j x =
      if j<y0 then x else
        if j>y1 then x else
          u x


toggle :: Point -> Point -> [[Bool]] -> [[Bool]]
toggle p0 p1 ls = applyInRegion not p0 p1 ls

turn :: Bool -> Point -> Point -> [[Bool]] -> [[Bool]]
turn b p0 p1 ls = applyInRegion (\_->b) p0 p1 ls

apply :: [[Bool]] -> (String, Point, Point) -> [[Bool]]
apply ls (c, p0, p1)
 | c=="turnon" = turn True p0 p1 ls
 | c=="turnoff" = turn False p0 p1 ls
 | c=="toggle" = toggle p0 p1 ls
 | otherwise = error ""

apply_all cs = foldl' apply lights cs

count_them ls = sum $ map (foldl' tick 0) ls
  where tick n b = if b then n+1 else n


solution1 cs = count_them $ apply_all cs

{-------------------------------  Part The 2nd  -------------------------------}

lights2 :: [[Int]]
lights2 = [[0 | y<-[0..999]]
              | x<-[0..999]]

turnon2 p0 p1 ls = applyInRegion (+1) p0 p1 ls
toggle2 p0 p1 ls = applyInRegion (+2) p0 p1 ls
turnoff2 p0 p1 ls = applyInRegion (\v->max 0 (v-1)) p0 p1 ls


apply2 :: [[Int]] -> (String, Point, Point) -> [[Int]]
apply2 ls (c, p0, p1)
 | c=="turnon" = turnon2 p0 p1 ls
 | c=="turnoff" = turnoff2 p0 p1 ls
 | c=="toggle" = toggle2 p0 p1 ls
 | otherwise = error ""

apply_all2 cs = foldl' apply2 lights2 cs

add_them ls = sum $ map sum ls

solution2 cs = add_them $ apply_all2 cs


--- IO ---

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
