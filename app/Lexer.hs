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
