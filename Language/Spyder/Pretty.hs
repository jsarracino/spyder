
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Spyder.Pretty (
  pretty
) where

import Language.Spyder.AST.Component

import Text.PrettyPrint.ANSI.Leijen


instance (Pretty Component) where
  pretty = error "todo"
