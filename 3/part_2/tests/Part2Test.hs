module Main where

import Part2
import Test.HUnit
import qualified System.Exit as Exit

testGetMuls :: Test
testGetMuls = TestList [
    "One valid" ~: getMuls "mul(1,2)" ~?= ["mul(1,2)"],
    "Empty string" ~: getMuls "" ~?= [],
    "Get multiple without space" ~: getMuls "mul(1,3)dumymul(999,1)" ~?= ["mul(1,3)", "mul(999,1)"],
    "Get multiple with space" ~: getMuls "mul(1,3) dumy mul(999,1)" ~?= ["mul(1,3)", "mul(999,1)"],
    "mul with zero at first place" ~: getMuls "mul(0,1)" ~?= [],
    "mul with zero at second place" ~: getMuls "mul(1,0)" ~?= [],
    "extra char inside" ~: getMuls "mul(1,g1)" ~?= [],
    "two follow up" ~: getMuls "mul(1,1)mul(999,999)" ~?= ["mul(1,1)", "mul(999,999)"],
    "multiple valid with spaces" ~: getMuls "mul(1,2) mul(3,4) mul(5,6)" ~?= ["mul(1,2)", "mul(3,4)", "mul(5,6)"],
    "multiple valid without spaces" ~: getMuls "mul(1,2)mul(3,4)mul(5,6)" ~?= ["mul(1,2)", "mul(3,4)", "mul(5,6)"],
    "mixed valid and invalid" ~: getMuls "mul(1,2) mul(0,4) mul(5,6)" ~?= ["mul(1,2)", "mul(5,6)"]
  ]


testGetLineMul :: Test
testGetLineMul = TestList [
    "One valid" ~: getLineMul "mul(1,2)" ~?= 2,
    "Zero at first place" ~: getLineMul "mul(0,2)" ~?= 0,
    "Zero at second place" ~: getLineMul "mul(1,0)" ~?= 0,
    "Two digits" ~: getLineMul "mul(10,20)" ~?= 200,
    "Three digits" ~: getLineMul "mul(100,200)" ~?= 20000
  ]

tests :: Test
tests = TestList [
    TestLabel "Test GetMuls" testGetMuls,
    TestLabel "Test GetLineMul" testGetLineMul
  ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
