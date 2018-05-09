module Language.Spyder.Synth.Context (
    Context(..)
  , buildContext
) where

import Language.Spyder.AST                    (Program(..))
import Language.Spyder.AST.Component          (Component(..))
import qualified Data.Map.Strict as Map

data Context = SynthCtx {
    varNames :: [String]
  , numConsts :: [Int]
} deriving (Eq, Show, Ord)

buildContext :: Program -> Map.Map String String -> Context
buildContext _ _ = SynthCtx names consts
  where
    names = ["Main$x", "Main$y"]
    consts = [1, 3, 7, 10, 0]