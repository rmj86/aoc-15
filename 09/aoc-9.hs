{------------------------------{ Advent of Code }------------------------------}
{-----------------------{ Day 9: All in a Single Night }-----------------------}
{-------------------------------{ Part the 1st }--------------------------------
  Every year, Santa manages to deliver all of his presents in a single night.
  
  This year, however, he has some new locations to visit; his elves have 
  provided him the distances between every pair of locations. He can start and 
  end at any two (different) locations he wants, but he must visit each 
  location exactly once. What is the shortest distance he can travel to 
  achieve this?
  
  For example, given the following distances:
  
      London to Dublin = 464
      London to Belfast = 518
      Dublin to Belfast = 141
  
  The possible routes are therefore:
  
      Dublin -> London -> Belfast = 982
      London -> Dublin -> Belfast = 605
      London -> Belfast -> Dublin = 659
      Dublin -> Belfast -> London = 659
      Belfast -> Dublin -> London = 605
      Belfast -> London -> Dublin = 982
  
  The shortest of these is London -> Dublin -> Belfast = 605, and so the 
  answer is 605 in this example.
  
  What is the distance of the shortest route?
-------------------------------------------------------------------------------}


{-------------------------------{ Part the 2nd }--------------------------------
  The next year, just to show off, Santa decides to take the route with the 
  longest distance instead.
  
  He can still start and and at any two (different) locations he wants, and he 
  still must visit each location exactly once.
  
  For example, given the distances above, the longest route would be 982 via 
  (for example) Dublin -> London -> Belfast.
  
  What is the distance of the longest route?
-------------------------------------------------------------------------------}

import Data.List (permutations, nub)
import Data.Map.Strict ( Map, (!), empty, insertWith, union, singleton
                       , keys, elems)

type Graph = Map String (Map String Int)

-- get list of nodes from graph
keys' g = nub . concat $ keys g : [keys g' | g' <- elems g]  

addEdge g (a,b,dist) = insertWith (union) x (singleton y dist) g
    where (x, y) = (min a b, max a b)

dist g (a,b) = g ! x ! y
    where (x, y) = (min a b, max a b)

pathDist g path = sum . map (dist g) . zip path . tail $ path

minPath g = minimum . map (pathDist g) . permutations
maxPath g = maximum . map (pathDist g) . permutations

{------------------------------------{ IO }------------------------------------}

parseLine line = (a, b, read dist)
    where [a,_,b,_,dist] = words line

getGraph = readFile "input.txt"
           >>= return . foldl addEdge empty . map parseLine . lines

main = do 
    g <- getGraph
    let ts = keys' g
    print $ minPath g ts
    print $ maxPath g ts
