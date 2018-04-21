module ParseTests (
  ptests
) where

import Test.HUnit
import Text.Parsec
import Language.Spyder.Parser.Parser (prog)


parseSucc :: FilePath -> Test
parseSucc s = TestCase $ readFile s >>= runner
  where 
    runner fle = case parse prog "" fle of 
      Right _ -> assert()
      Left _ -> assertFailure $ "couldn't parse: " ++ s
  

parseFail :: String -> Test
parseFail s = TestCase $ readFile s >>= runner
  where 
    runner fle = case parse prog "" fle of 
      Left _ -> assert()
      Right _ -> assertFailure $ "shouldn't have parsed: " ++ s

unitPrefix = "./test/bench/spy/unit/"
unitSuccPrefix = unitPrefix ++ "good/"
unitSuccBenches = TestList benches
  where
    small = ["empty", "array", "num", "two-data", "proc-empty", "proc-args", "rel", "always", "using", "two-comp"]
    benchnames = map (\x -> unitSuccPrefix ++ x ++ ".spy") small
    benches = map parseSucc benchnames

ptests = TestList [unitSuccBenches]