module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment (getArgs)

import Lexer (runAlex)
import Parser (parseMain)
import Checks
import Typecheck
import Desugar

main :: IO ()
main = do
  args <- getArgs
  code <- BS.readFile (head args)
  case runAlex code parseMain of
    Left err -> putStrLn err
    Right prog -> case runChecks prog of
      [] -> case inferTop prog of
        Left errs -> print "Type Errors" >> mapM_ print errs
        Right typed -> print (desugar typed)
      errs -> print "Semantic Errors" >> mapM_ print errs
