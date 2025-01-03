module Part1 where
    
import Text.Regex.TDFA
import Text.Read (readMaybe)

getMuls :: String -> [String]
getMuls content = getAllTextMatches (content =~ mulRegex :: AllTextMatches [] String)
    where mulRegex = "mul\\([1-9][0-9]{0,2},[1-9][0-9]{0,2}\\)"

getLineMul :: String -> Int
getLineMul line = case (readMaybe a, readMaybe b) of
    (Just x, Just y) -> x * y
    _ -> 0
  where
    a = takeWhile (/= ',') $ drop 4 line
    b = takeWhile (/= ')') $ drop 1 $ dropWhile (/= ',') line