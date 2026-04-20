module Parser where

import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe)
import Lexer (Token (Newline, RequestSeperator))

data Root = Root
  { requests :: [RequestExpression]
  }
  deriving (Show)

data MethodStatement = Get | Post | Put | Patch | Delete | Head | Connect | Options | Trace
  deriving (Show)

data TargetStatement = AbsoluteTarget String | OriginTarget String
  deriving (Show)

data RequestLineExpression = RequestLineExpression
  { method :: MethodStatement,
    target :: TargetStatement,
    httpVersion :: String
  }
  deriving (Show)

data RequestExpression = RequestExpression
  {requestLineExpression :: RequestLineExpression}
  deriving (Show)

parse :: [Token] -> Root
parse tokens =
  Root
    { requests = map parseRequest reqs
    }
  where
    reqs = splitOnSeperator tokens

parseRequest :: [Token] -> RequestExpression
parseRequest tokens =
  RequestExpression
    { requestLineExpression = parseRequestLine (listToMaybe lines)
    }
  where
    lines = splitOnLineEnding tokens

parseRequestLine :: Maybe [Token] -> RequestLineExpression
parseRequestLine Nothing = error "Missing request line"
parseRequestLine (Just tokens) = error "not implemented"

splitOnSeperator :: [Token] -> [[Token]]
splitOnSeperator tokens = splitOn [RequestSeperator] tokens

splitOnLineEnding :: [Token] -> [[Token]]
splitOnLineEnding tokens = splitOn [Newline] tokens
