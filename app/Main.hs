module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment (getArgs)

import Frontend

main :: IO ()
main = do
  args <- getArgs
  code <- BS.readFile (head args)
  case toCore code of
    Left err -> print err
    Right prog -> print prog
