module Parser where

import Data.Data (Data (toConstr), Typeable, showConstr)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, listToMaybe)
import Debug.Trace (traceShow)
import Lexer (Token (Character, Newline, RequestSeperator, Whitespace), isComment, isNewline, isWhitespace, unlex)
import Network.URI (URI, parseURIReference, uriIsAbsolute)
import Text.Read (Lexeme (Char))

data Root = Root
  { requests :: [RequestExpression]
  }
  deriving (Show)

data MethodStatement = Get | Post | Put | Patch | Delete | Head | Connect | Options | Trace
  deriving (Show, Eq, Typeable)

data TargetStatement = AbsoluteTarget URI | OriginTarget URI
  deriving (Show)

data RequestLineExpression = RequestLineExpression
  { method :: MethodStatement,
    target :: TargetStatement,
    httpVersion :: String
  }
  deriving (Show)

data HeaderFieldExpression = HeaderFieldExpression
  { name :: String,
    value :: String
  }
  deriving (Show, Eq)

data MessageStatement = InlineMessageStatement String | InputFileRef String deriving (Show)

data MessageBodyExpression = MessageBodyExpression
  { message :: MessageStatement
  }
  deriving (Show)

data RequestExpression = RequestExpression
  { requestLine :: RequestLineExpression,
    headers :: [HeaderFieldExpression],
    messageBody :: MessageBodyExpression
  }
  deriving (Show)

defaultGet :: [Token]
defaultGet = [Character 'G', Character 'E', Character 'T']

defaultVersion :: [Token]
defaultVersion = [Character 'H', Character 'T', Character 'T', Character 'P', Character '/', Character '1', Character '.', Character '1']

parseMethod :: [Token] -> MethodStatement
parseMethod tokens =
  case unlex tokens of
    "GET" -> Get
    "POST" -> Post
    "PUT" -> Put
    "PATCH" -> Patch
    "DELETE" -> Delete
    "HEAD" -> Head
    "CONNECT" -> Connect
    "OPTIONS" -> Options
    "TRACE" -> Trace
    _ -> error "Invalid method"

trimStart :: [Token] -> [Token]
trimStart tokens = case tokens of
  (Whitespace : t) -> t
  _ -> tokens

trimEnd :: [Token] -> [Token]
trimEnd tokens = reverse $ trimStart (reverse tokens)

trimTokens :: [Token] -> [Token]
trimTokens tokens = (trimStart . trimEnd) tokens

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
    { requestLine = case rl of
        (Just l) -> parseRequestLine l
        Nothing -> error "Missing request line",
      headers = map parseHeaderLine hl,
      messageBody = parseMessage b
    }
  where
    l = filter (\li -> length li > 0) (splitOnLineEnding tokens)
    rl = listToMaybe l
    hl = takeWhile (\li -> Character ':' `elem` li) (drop 1 l)
    b = drop (length hl + 1) l

parseMessage :: [[Token]] -> MessageBodyExpression
parseMessage tokens = MessageBodyExpression {message = InlineMessageStatement $ unlex (concat tokens)}

parseVersion :: [Token] -> String
parseVersion tokens = unlex tokens

parseHeaders :: [[Token]] -> [HeaderFieldExpression]
parseHeaders lines = map parseHeaderLine lines

parseHeaderLine :: [Token] -> HeaderFieldExpression
parseHeaderLine line = case parts of
  [name, value] ->
    HeaderFieldExpression
      { name = unlex name,
        value = unlex $ trimTokens value
      }
  _ -> error "Invalid header"
  where
    parts = splitOn [(Character ':')] line

parseRequestLine :: [Token] -> RequestLineExpression
parseRequestLine tokens = parseRequestLine_ expr
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
    u = fromJust $ parseURIReference (unlex uTok)
parseRequestLine_ [mTok, uTok] = parseRequestLine_ [mTok, uTok, defaultVersion]
parseRequestLine_ [uTok] = parseRequestLine_ [defaultGet, uTok, defaultVersion]
parseRequestLine_ _ = error "Invalid length"

splitOnSeperator :: [Token] -> [[Token]]
splitOnSeperator tokens = filter isEmptyRequestBlock (splitOn [RequestSeperator] tokens)

splitOnLineEnding :: [Token] -> [[Token]]
splitOnLineEnding tokens = splitOn [Newline] tokens

isEmptyRequestBlock :: [Token] -> Bool
isEmptyRequestBlock tokens = not (all (\t -> isNewline t || isWhitespace t || isComment t) tokens)

methodStatementToString :: MethodStatement -> String
methodStatementToString (Get) = "GET"
methodStatementToString (Post) = "POST"
methodStatementToString (Put) = "PUT"
methodStatementToString (Patch) = "PATCH"
methodStatementToString (Delete) = "DELETE"
methodStatementToString (Head) = "HEAD"
methodStatementToString (Connect) = "CONNECT"
methodStatementToString (Options) = "OPTIONS"
methodStatementToString (Trace) = "TRACE"
