main :: IO()
main = do
  fileContent <- readFile "input.txt"
  let records = getRecords fileContent
  let alternativeRecords = map createAlternativeRecords records
  let recordsValidationList = map checkAllPosibleRecords alternativeRecords
  let numberOfValidRecords = count True recordsValidationList
  print numberOfValidRecords 


getRecords :: String -> [[Int]]
getRecords content = map convertLineToRecord $ lines content
  where convertLineToRecord line = map read $ words line


createAlternativeRecords :: [Int] -> [[Int]]
createAlternativeRecords record = [record] ++ [take i record ++ drop (i + 1) record | i <- [0 .. length record - 1]]


checkAllPosibleRecords :: [[Int]] -> Bool
checkAllPosibleRecords [] = False
checkAllPosibleRecords (x:xs)
  | checkRecordChanges $ calculateLevelChanges x = True
  | otherwise = checkAllPosibleRecords xs


calculateLevelChanges :: [Int] -> [Int]
calculateLevelChanges record = zipWith (-) (tail record) record


checkRecordChanges :: [Int] -> Bool
checkRecordChanges levelChanges = 
  let 
    monoton = checkMonotonity levelChanges
    changeValid = checkChangesVolume levelChanges
  in
    and [monoton, changeValid]


checkMonotonity :: [Int] -> Bool
checkMonotonity levelChanges =
  let 
    allPositive = all (>0) levelChanges
    allNegativ = all (<0) levelChanges
  in
    or [allPositive, allNegativ]


-- We check just if the changes are less or equal than 3 
-- We already checked if the change is bigger than 0 in the monotonity check
checkChangesVolume :: [Int] -> Bool
checkChangesVolume levelChanges = all (<=3) $ map abs levelChanges


count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)
