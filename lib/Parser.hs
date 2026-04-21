module Parser where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust, listToMaybe)
import Lexer (Token (Character, Newline, RequestSeperator, Whitespace), isComment, isNewline, isWhitespace, unwrapChar)
import Network.URI (URI, parseURIReference, uriIsAbsolute)

data Root = Root
  { requests :: [RequestExpression]
  }
  deriving (Show)

data MethodStatement = Get | Post | Put | Patch | Delete | Head | Connect | Options | Trace
  deriving (Show, Eq)

defaultGet :: [Token]
defaultGet = [Character 'G', Character 'E', Character 'T']

defaultVersion :: [Token]
defaultVersion = [Character '1']

parseMethod :: [Token] -> MethodStatement
parseMethod tokens =
  case mapM unwrapChar tokens of
    Just "GET" -> Get
    Just "POST" -> Post
    Just "PUT" -> Put
    Just "PATCH" -> Patch
    Just "DELETE" -> Delete
    Just "HEAD" -> Head
    Just "CONNECT" -> Connect
    Just "OPTIONS" -> Options
    Just "TRACE" -> Trace
    _ -> error "Invalid method"

parseVersion :: [Token] -> String
parseVersion _ = "V1"

data TargetStatement = AbsoluteTarget URI | OriginTarget URI
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
    { requestLineExpression = parseRequestLine (listToMaybe l)
    }
  where
    l = filter (\li -> length li > 0) (splitOnLineEnding tokens)

parseRequestLine :: Maybe [Token] -> RequestLineExpression
parseRequestLine Nothing = error "Missing request line"
parseRequestLine (Just tokens) = parseRequestLine_ expr
  where
    expr = splitOn [Whitespace] tokens

parseRequestLine_ :: [[Token]] -> RequestLineExpression
parseRequestLine_ [mTok, uTok, vTok] =
  RequestLineExpression
    { method = parseMethod mTok,
      target = case uriIsAbsolute u of
        True -> AbsoluteTarget u
        False -> OriginTarget u,
      httpVersion = parseVersion vTok
    }
  where
    u = case mapM unwrapChar uTok of
      Just str -> fromJust (parseURIReference str)
      Nothing -> error "Parse error: Invalid characters in url"
parseRequestLine_ [mTok, uTok] = parseRequestLine_ [mTok, uTok, defaultVersion]
parseRequestLine_ [uTok] = parseRequestLine_ [defaultGet, uTok, defaultVersion]
parseRequestLine_ _ = error "Invalid length"

splitOnSeperator :: [Token] -> [[Token]]
splitOnSeperator tokens = filter isEmptyRequestBlock (splitOn [RequestSeperator] tokens)

splitOnLineEnding :: [Token] -> [[Token]]
splitOnLineEnding tokens = splitOn [Newline] tokens

isEmptyRequestBlock :: [Token] -> Bool
isEmptyRequestBlock tokens = not (all (\t -> isNewline t || isWhitespace t || isComment t) tokens)
