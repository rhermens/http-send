module Main where

import Lexer (scanTokens)
import Options.Applicative (Parser, argument, execParser, helper, info, metavar, progDesc, str, (<**>))
import Parser (parse)
import System.IO (IOMode (ReadMode), hGetContents, withFile)

data Args = Args
  { httpFile :: String
  }

args :: Parser Args
args =
  Args
    <$> argument str (metavar "HTTP_FILE")

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (args <**> helper)
        (progDesc "Execute http file")

run :: Args -> IO ()
run a =
  withFile
    (httpFile a)
    ReadMode
    ( \hdl -> do
        contents <- hGetContents hdl
        let tokens = scanTokens contents []
        print (parse tokens)
    )
