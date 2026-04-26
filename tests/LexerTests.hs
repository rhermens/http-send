module LexerTests where

import Lexer (Token (Character, Whitespace), unlex)
import Test.HUnit
import Test.HUnit.Base

showTokens :: Test
showTokens = TestCase (assertEqual "String matches tokens" "A BC" (unlex [Character 'A', Whitespace, Character 'B', Character 'C']))

lexerTests :: Test
lexerTests = TestList [TestLabel "Show instance [token]" showTokens]
