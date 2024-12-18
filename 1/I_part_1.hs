import System.IO
import Data.List


extractList :: String -> ([Int], [Int]) 
extractList content = unzip $ map parseLists $ lines content
  where parseLists line = case words line of
          [x, y] -> (read x, read y)
          _      -> error "Invalid format"


calculateDistance :: [Int] -> [Int] -> Int
calculateDistance firstList secondList = 
  let sortedFirst  = sort firstList
      sortedSecond = sort secondList
  in
    sum $ zipWith (\a b -> abs $ a - b) sortedFirst sortedSecond


main :: IO() 
main = do 
  fileContent <- readFile "lists.txt" 
  let (listOne, listTwo) = extractList fileContent
  let distance = calculateDistance listOne listTwo
  print distance
