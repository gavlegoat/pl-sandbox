module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment (getArgs)

import Lexer (runAlex)
import Parser (parseMain)

main :: IO ()
main = do
  args <- getArgs
  code <- BS.readFile (head args)
  case runAlex code parseMain of
    Left err -> putStrLn err
    Right ast -> print ast
