module Main where

import Test.HUnit

import ParserTest

main :: IO Counts
main = do
  runParseTests

