import Data.List
import Data.List.Split
import System.IO

main :: IO ()
main = do
  s <- lines <$> readFile "input1"
  let input = init s

  let dimensions = map (\x -> map (\y -> read y :: Int) (splitOn "x" x)) input

  let solution = foldl' (\acc curr -> acc + box curr) 0 $ dimensions

  let solution2 = foldl' (\acc curr -> acc + ribbon curr) 0 $ dimensions

  print solution
  print solution2
  where
    box :: [Int] -> Int
    box [l, w, h] = (2 * l * w) + (2 * w * h) + (2 * h * l) + minimum [l * w, w * h, h * l]

    ribbon :: [Int] -> Int
    ribbon [l, w, h] = ((2 * l + 2 * w + 2 * h) - (2 * maximum [l, w, h])) + (l * w * h)
