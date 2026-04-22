module Lexer where

data Token = RequestSeperator | Comment | Whitespace | Newline | Character Char
  deriving (Show, Eq)

scanTokens :: String -> [Token] -> [Token]
scanTokens "" tok = reverse tok
scanTokens ('#' : '#' : '#' : rest) tok = scanTokens (drop 1 (dropWhile (/= '\n') rest)) (RequestSeperator : tok)
scanTokens ('#' : rest) tok = scanTokens rest (Comment : tok)
scanTokens (' ' : rest) tok = scanTokens rest (Whitespace : tok)
scanTokens ('\n' : rest) tok = scanTokens rest (Newline : tok)
scanTokens (t : rest) tok = scanTokens rest (Character t : tok)

unwrapChar :: Token -> Maybe Char
unwrapChar (Character c) = Just c
unwrapChar Whitespace = Just ' '
unwrapChar _ = Nothing

charsIntoString :: [Token] -> String
charsIntoString tokens =
  case mapM unwrapChar tokens of
    Just x -> x
    _ -> error "Invalid character sequence"

isNewline :: Token -> Bool
isNewline token = case token of
  Newline -> True
  _ -> False

isWhitespace :: Token -> Bool
isWhitespace token = case token of
  Whitespace -> True
  _ -> False

isComment :: Token -> Bool
isComment token = case token of
  Comment -> True
  _ -> False
