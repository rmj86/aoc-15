{------------------------------{ Advent of Code }------------------------------}
{--------------{ Day 5: Doesn't He Have Intern-Elves For This? }---------------}
{-------------------------------{ Part the 1st }--------------------------------
  Santa needs help figuring out which strings in his text file are naughty or 
  nice.
  
  A nice string is one with all of the following properties:
  
    - It contains at least three vowels (aeiou only), like aei, xazegov, 
      or aeiouaeiouaeiou.
    - It contains at least one letter that appears twice in a row, like xx, 
      abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
    - It does not contain the strings ab, cd, pq, or xy, even if they are part 
      of one of the other requirements.
  
  For example:
  
    - ugknbfddgicrmopn is nice because it has at least three vowels 
      (u...i...o...), a double letter (...dd...), and none of the disallowed
      substrings.
    - aaa is nice because it has at least three vowels and a double letter, 
      even though the letters used by different rules overlap.
    - jchzalrnumimnmhp is naughty because it has no double letter.
    - haegwjzuvuyypxyu is naughty because it contains the string xy.
    - dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?

-------------------------------------------------------------------------------}

import Data.List (partition, tails)

hasVowels s = ((>=3).length.fst.(partition (`elem` "aeiou"))) s

pairs s = zipWith (\a b->[a,b]) s (tail s)

hasPair s = any (\[a,b]->a==b) (pairs s)

hasNoNoNo s = not (any (`elem` ["ab","cd","pq","xy"]) (pairs s))

isNice s = hasVowels s && hasPair s && hasNoNoNo s

solution1 ss = length (filter isNice ss)

{------------------------------{ Part The 2nd }---------------------------------
  Realizing the error of his ways, Santa has switched to a better model of 
  determining whether a string is naughty or nice. None of the old rules 
  apply, as they are all clearly ridiculous.
  
  Now, a nice string is one with all of the following properties:
  
    - It contains a pair of any two letters that appears at least twice in 
      the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but 
      not like aaa (aa, but it overlaps).
    - It contains at least one letter which repeats with exactly one letter 
      between them, like xyx, abcdefeghi (efe), or even aaa.
  
  For example:
  
    - qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) 
      and a letter that repeats with exactly one letter between them (zxz).
    - xxyxx is nice because it has a pair that appears twice and a letter that
      repeats with one between, even though the letters used by each rule 
      overlap.
    - uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat 
      with a single letter between them.
    - ieodomkazucvgmuy is naughty because it has a repeating letter with one 
      between (odo), but no pair that appears twice.
  
  How many strings are nice under these new rules?
-------------------------------------------------------------------------------}

triplets s = zip3 s (tail s) (tail (tail s))

hasPairTwice s = any f (tails (pairs s))
  where f (t:u:us) = t `elem` us
        f _ = False

hasAba s = any (\(a,b,c)->a==c) (triplets s)

isNice2 s = hasAba s && hasPairTwice s

solution2 ss = length (filter isNice2 ss)

{------------------------------------- IO -------------------------------------}

getData :: IO [String]
getData = do s <- readFile "data/aoc-05.txt" 
             return $ lines s

main :: IO ()
main = do ss <- getData
          print (solution1 ss)
          print (solution2 ss)
