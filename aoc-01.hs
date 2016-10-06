{-----------------------{ Advent of Code, December 1st }-----------------------}
{-------------------------------{ Part the 1st }--------------------------------
  Santa is trying to deliver presents in a large apartment building, but he 
  can't find the right floor - the directions he got are a little confusing. 
  He starts on the ground floor (floor 0) and then follows the instructions 
  one character at a time.
  
  An opening parenthesis, (, means he should go up one floor, and a closing 
  parenthesis, ), means he should go down one floor.
  
  The apartment building is very tall, and the basement is very deep; he will 
  never find the top or bottom floors.
  
  For example:
  
      (()) and ()() both result in floor 0.
      ((( and (()(()( both result in floor 3.
      ))((((( also results in floor 3.
      ()) and ))( both result in floor -1 (the first basement level).
      ))) and )())()) both result in floor -3.
  
  To what floor do the instructions take Santa?
-------------------------------------------------------------------------------}

valueOf :: Char -> Int
valueOf '(' = 1
valueOf ')' = -1
valueOf _ = 0

solution1 :: String -> Int
solution1 s = foldl (\n c -> n+valueOf c) 0 s

{------------------------------{ Part The 2nd }---------------------------------
  Now, given the same instructions, find the position of the first character 
  that causes him to enter the basement (floor -1). The first character in the 
  instructions has position 1, the second character has position 2, and so on.
  
  For example:
  
      ) causes him to enter the basement at character position 1.
      ()()) causes him to enter the basement at character position 5.
  
  What is the position of the character that causes Santa to first enter the 
  basement?
-------------------------------------------------------------------------------}

solution2 s = length (takeWhile (>=0) position)
  where position = scanl (\n c->n+valueOf c) 0 s

{------------------------------------- IO -------------------------------------}

getData :: IO String
getData = readFile "data/aoc-01.txt"

main :: IO ()
main = do s <- getData
          print (solution1 s)
          print (solution2 s)
