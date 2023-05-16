module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment (getArgs)

import Lexer (runAlex)
import Parser (parseMain)
-- import Interpreter
import Typecheck

main :: IO ()
main = do
  args <- getArgs
  code <- BS.readFile (head args)
  case runAlex code parseMain of
    Left err -> putStrLn err
    Right expr -> case inferTypes expr of
      Left errs -> print "Type Errors" >> mapM_ print errs
      Right typed -> print typed
