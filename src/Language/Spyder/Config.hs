module Language.Spyder.Config (
    concretSize
  , concretSizeRef
)

where

import System.IO.Unsafe
import Data.IORef

{-# NOINLINE concretSizeRef #-}
concretSizeRef :: IORef Int
concretSizeRef = unsafePerformIO $ newIORef 4

{-# NOINLINE concretSize #-}
concretSize :: Int
concretSize = unsafePerformIO $ readIORef concretSizeRef