import Data.List (nub)


count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)


calculateSimilarity :: [Int] -> [Int] -> Int
calculateSimilarity firstList secondList = sum $ map calcKeySimilarity $ nub firstList
  where calcKeySimilarity key = key * (count key firstList) * (count key secondList)


extractLists :: String -> ([Int], [Int])
extractLists fileContent = unzip $ map parseLine $ lines fileContent
  where parseLine line = case words line of
          [a, b] -> (read a, read b)
          _      -> error "Invalid"


main ::IO()
main = do
  fileContent <- readFile "lists.txt"
  let (firstList, secondList) = extractLists fileContent
  print $ calculateSimilarity firstList secondList
