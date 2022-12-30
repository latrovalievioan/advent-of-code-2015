import System.IO  
import Data.List

main :: IO()
main = do
  s <- readFile "input1"
  let input = init s
  
  let (r, l) = foldl' (\(fs, sn) curr -> if curr == '(' then (fs + 1, sn) else (fs, sn + 1 )) (0, 0) input

  print $ (r - l)
