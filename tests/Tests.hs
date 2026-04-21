{-# LANGUAGE QuasiQuotes #-}

module Main where

import GHC.IO.Exception (ExitCode)
import Lexer (Token (Character, Newline, RequestSeperator), scanTokens)
import Network.URI (URI (URI, uriPath))
import Parser (MethodStatement (Get, Post), RequestLineExpression (RequestLineExpression, httpVersion, method, target), TargetStatement (OriginTarget), parseRequestLine_, splitOnSeperator)
import System.Exit qualified as Exit
import Test.HUnit (Assertable (assert), Counts (failures), Test (TestCase, TestLabel, TestList), Testable (test), assertEqual, runTestTT)
import Text.RawString.QQ
import Text.Read (Lexeme (Char))

testParseRequestLine_ :: Test
testParseRequestLine_ = TestCase (assertEqual "Should parse into request line" Post (method (parseRequestLine_ [[Character 'P', Character 'O', Character 'S', Character 'T'], [Character 'a']])))

testSplitOnSeperator :: Test
testSplitOnSeperator =
  TestCase
    ( assertEqual
        "Should have length of 1"
        1
        ( length
            ( splitOnSeperator
                ( scanTokens
                    [r|
###

POST http://example.com/api/add HTTP/1.1
Content-Type: application/json

{
  "name": "entity",
  "value": "content"
}

###
|]
                    []
                )
            )
        )
    )

tests :: Test
tests = TestList [TestLabel "testParseRequestLine_" testParseRequestLine_, TestLabel "testSplitOnSeperator" testSplitOnSeperator]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
