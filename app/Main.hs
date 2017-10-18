{-# LANGUAGE FlexibleContexts, QuasiQuotes, TemplateHaskell #-}
module Main where
import System.Environment(getArgs)
import System.Exit
import Lib
import Data.List

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "spf-converter between a lambda-calculus-like representation and spf's representation"
      putStrLn ""
      putStrLn "spf-converter spf '<lambda-expression>'"
      putStrLn "spf-converter lambda '<spf-expression>'"
      exitSuccess
    ("spf":s:[]) -> do
      putStrLn $ showSPF $ parse lambdaP s
      exitSuccess
    ("lambda":s:[]) -> do
      putStrLn $ showLambda $ parse spfP s
      exitSuccess
    l -> do
      print "Unknown arguments"
      print l
      exitFailure
