{-# LANGUAGE OverloadedStrings #-}

module Client where

import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy.Char8 qualified as BL
import Debug.Trace (traceShow)
import Network.HTTP.Conduit (Request, defaultRequest, http, parseRequest)
import Network.HTTP.Simple (getResponseHeader, getResponseStatusCode, httpLBS, parseRequest, setRequestBodyLBS, setRequestHost, setRequestMethod, setRequestPath, setRequestPort, setRequestSecure)
import Network.URI (URI (uriAuthority, uriPath, uriQuery), uriToString)
import Parser (MessageBodyExpression (message), RequestExpression (messageBody, requestLine), RequestLineExpression (..), TargetStatement (AbsoluteTarget, OriginTarget), methodStatementToString)
import Parser qualified as B

data Response = Response

send :: RequestExpression -> IO ()
send expr =
  do
    request' <- buildRequest $ requestLine expr
    let request =
          setRequestBodyLBS (BL.pack (show (message (messageBody expr)))) $ request'

    response <- traceShow request $ httpLBS request
    print $ getResponseStatusCode response

buildRequest :: RequestLineExpression -> IO Request
buildRequest stmt = parseRequest (exprIntoRequest stmt)

exprIntoRequest :: RequestLineExpression -> String
exprIntoRequest expr = (methodStatementToString (method expr)) ++ " " ++ ((uriToString id (takeU (target expr))) "")
  where
    takeU (AbsoluteTarget u) = u
    takeU (OriginTarget u) = u
