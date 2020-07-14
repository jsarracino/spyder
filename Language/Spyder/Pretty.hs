
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Spyder.Pretty (
  pretty
) where

import Language.Spyder.AST

import Text.PrettyPrint.ANSI.Leijen


instance (Pretty Program) where
  pretty = error "todo"
