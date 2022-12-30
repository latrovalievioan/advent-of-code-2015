import System.IO  
import Data.List

main :: IO()
main = do
  s <- readFile "input1"
  let input = init s
  
  let test = "()())"
  
  let solution = foldl' step (0, 0, 0) input

  print solution

  where
    step (acc, i, found) curr
      | curr == '(' = (acc + 1, i + 1, found)
      | otherwise = if (acc == -1 && found == 0) then (acc - 1, i + 1, i) else (acc - 1, i + 1, found)
