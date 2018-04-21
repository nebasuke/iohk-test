module ParserTest where

import Test.HUnit

import NodeListParser (parseEndPoints)
import Types

parseEndPoints1 = TestCase (
  do
    input <- readFile "test/nodelist-whitespace.txt"
    assertEqual
      "Endpoints from file are parsed correctly: "
      (Right [EndPoint "127.0.0.1" 1234,EndPoint "test.domain.com" 443,EndPoint "127.0.0.1" 333])
      (parseEndPoints input)
  )

parserTests = TestList [TestLabel "Parse end points with starting and other whitespace" parseEndPoints1]
