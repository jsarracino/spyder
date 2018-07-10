#!/usr/bin/env stack
{- stack
  script
  --resolver lts-2.0
  --package z3-4.3.1
  --package lens-4.13
  --package random-1.0.0.0
  --package mtl-2.1.2
  --package ansi-wl-pprint-0.6.4
  --package html-1.0
  --package parsec-3.1.0
  --package ansi-terminal-0.5.0
  --package stream-monad-0.4.0.2
  --package base-orphans-0.4.0
  --package tagged-0.7.3
  --package adjunctions-4.2.1
  --package free-4.12
  --package bifunctors-5
  --package profunctors-5
  --package semigroupoids-5
  --package comonad-4.2.7.1
  --package kan-extensions-4.2.2
  --package reflection-2.1
  --package nats-0.1
  --package logict-0.5.0.2
  --package text-0.11.3.1
  --package intero-0.1.24
  --package ./boogaloo
-}

import qualified Language.Spyder as Spy

main = Spy.main