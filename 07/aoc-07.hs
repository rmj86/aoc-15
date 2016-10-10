{------------------------------{ Advent of Code }------------------------------}
{----------------------{ Day 7: Some Assembly Required }-----------------------}

-- Figuring out variable dependencies is what the Haskell compiler is for.
-- The obvious approach here is to transform the input to Haskell syntax and
-- compile it. This file generates the source files  aoc-07-p1.hs  and
-- aoc-07-p2.hs

import System.Process (system, readProcess)

{---------------------------------{ Parsing }----------------------------------}

parseLine l = unwords $ case words l of
        [              v2, "->", v3] -> [v_ v3, "=",               v_ v2]
        [    "NOT",    v2, "->", v3] -> [v_ v3, "=", "complement", v_ v2]
        [v1, "AND",    v2, "->", v3] -> [v_ v3, "=", v_ v1, ".&.", v_ v2]
        [v1, "OR",     v2, "->", v3] -> [v_ v3, "=", v_ v1, ".|.", v_ v2]
        [v1, "LSHIFT", v2, "->", v3] -> [v_ v3, "=", v_ v1, ".<.", v_ v2]
        [v1, "RSHIFT", v2, "->", v3] -> [v_ v3, "=", v_ v1, ".>.", v_ v2]
        _  -> error $ "Unparsable line: " ++ l
        -- _ -> [""]
  -- some of the identifiers are not legal Haskell names. Fix: prepend "v_"
  where v_ str = case reads str :: [(Int,String)] of
                        [(_,"")] -> str          -- is an Int literal
                        _        -> "v_" ++ str  -- is a name

{-------------------------------{ Part the 1st }-------------------------------}
generateP1 = return . map parseLine . lines =<< readFile "input.txt"

{-------------------------------{ Part the 2nd }-------------------------------}
replaceB new_b = map $ (\l -> case words l of
        ["v_b", "=", _]  -> unwords ["v_b", "=", new_b]
        _                -> l  )

{------------------------------------{ IO }------------------------------------}

writeProgram name p_lines = writeFile name . unlines
                          $ p_start ++ p_lines ++ p_main
  where p_start = [ "import Data.Bits"
                  , "import Data.Word"
                  , "(.<.) = shiftL"
                  , "(.>.) = shiftR"
                  , "v_a::Word16"
                  , "" ]
        p_main = ["", "main = print v_a", ""]

main = do
    p_lines <- generateP1
    writeProgram "aoc-07-p1.hs" p_lines
    system "ghc --make aoc-07-p1.hs"
    v_a <- readProcess "aoc-07-p1.exe" [] ""
    putStr "Part 1: " >> putStrLn v_a

    let p_lines2 = replaceB v_a p_lines
    writeProgram "aoc-07-p2.hs" p_lines2
    system "ghc --make aoc-07-p2.hs"
    v_a <- readProcess "aoc-07-p2.exe" [] ""
    putStr "Part 2: " >> putStrLn v_a
