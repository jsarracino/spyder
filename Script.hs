#!/usr/bin/env stack
{- stack
  --resolver lts-2.0
  --stack-yaml stack.yaml
  exec ghci
  --package .
-}

import qualified Language.Spyder as Spy

main = Spy.main