import Data.Word
import Data.Bits
{------------------------------{ Advent of Code }------------------------------}
{----------------------{ Day 7: Some Assembly Required }-----------------------}
{-------------------------------{ Part the 1st }-------------------------------}
{-------------------------------{ Part the 2nd }-------------------------------}

-- Figuring out variable dependencies is what the Haskell compiler is for.
-- The easiest approach here is to transform the input to Haskell syntax and
-- compile it. This file generates the source files  aoc-07-p1.hs  and
-- aoc-07-p2.hs

{---------------------------------{ Parsing }----------------------------------}

parseData = map parse1

parse1 s = (pv p3) ++ " = " ++ (parse2 p1)
  where (p1, p2) = break (=='-') s
        p3 = drop 3 p2

parse2 s
 | length w==1 = pv (w!!0)
 | w!!0=="NOT" = "complement " ++ pv (w!!1)
 | w!!1=="AND" = pv (w!!0) ++ " .&. " ++ pv (w!!2)
 | w!!1=="OR"  = pv (w!!0) ++ " .|. " ++ pv (w!!2)
 | w!!1=="LSHIFT" = "shiftL " ++ pv (w!!0) ++ " " ++ pv (w!!2)
 | w!!1=="RSHIFT" = "shiftR " ++ pv (w!!0) ++ " " ++ pv (w!!2)
 | otherwise = error s
 where w = words s

pv s = if elem s ["do","if","id","in"] then "var_"++s else s


{------------------------------------{ IO }------------------------------------}

readData = do s <- readFile "input.txt"
              return $ lines s

prgmStart = "import Data.Bits\nimport Data.Word\na::Word16\n\n"
prgmMain = "\nmain = print a\n"

writeData ss = do let prgm = prgmStart ++ unlines ss ++ prgmMain
                  writeFile "aoc-07-p1.hs" prgm


main = do ss <- readData
          let ts = parseData ss
          writeData ts
