module Main where

import Test.HUnit

import MessageTest
import ParserTest

main :: IO Counts
main = do
  runTestTT $ TestList [messageTests, parserTests]

