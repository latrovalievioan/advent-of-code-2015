import Data.List
import Data.List.Split
import System.IO

main :: IO ()
main = do
  s <- lines <$> readFile "input1"
  let input = init s

  let dimensions = map (\x -> map (\y -> read y :: Int) (splitOn "x" x)) input

  let solution = foldl' (\acc curr -> acc + box curr) 0 $ dimensions

  print solution
  where
    box :: [Int] -> Int
    box [l, w, h] = (2 * l * w) + (2 * w * h) + (2 * h * l) + minimum [l * w, w * h, h * l]
