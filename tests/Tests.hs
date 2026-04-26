{-# LANGUAGE QuasiQuotes #-}

module Main where

import GHC.IO.Exception (ExitCode)
import Lexer (Token (Character, Newline, RequestSeperator), scanTokens)
import LexerTests (lexerTests)
import Network.URI (URI (URI, uriPath))
import Parser (HeaderFieldExpression (HeaderFieldExpression, name, value), MethodStatement (Get, Post), RequestLineExpression (RequestLineExpression, httpVersion, method, target), TargetStatement (OriginTarget), parseHeaderLine, parseRequestLine, parseRequestLine_, splitOnSeperator)
import System.Exit qualified as Exit
import Test.HUnit (Assertable (assert), Counts (failures), Test (TestCase, TestLabel, TestList), Testable (test), assertEqual, runTestTT)
import Text.RawString.QQ
import Text.Read (Lexeme (Char))

testParseRequestLine :: Test
testParseRequestLine = TestCase (assertEqual "Should parse into request line" Post (method (parseRequestLine (scanTokens "POST http://test" []))))

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

testParseHeaderLine :: Test
testParseHeaderLine = TestCase (assertEqual "Parsed header matches" HeaderFieldExpression {name = "Content-Type", value = "application/json"} (parseHeaderLine (scanTokens "Content-Type: application/json" [])))

parseTests :: Test
parseTests = TestList [TestLabel "testParseRequestLine" testParseRequestLine, TestLabel "testSplitOnSeperator" testSplitOnSeperator, TestLabel "testParseHeaderLine" testParseHeaderLine]

tests :: Test
tests = TestList [parseTests, lexerTests]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
