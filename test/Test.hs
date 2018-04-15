module Test (
  hs,
  main
) where

import ParseTests (ptests)
import Test.HUnit

tests = TestList [TestLabel "Parse Tests" ptests]

main = runTestTT tests
hs = runTestTT tests
