-- File generated by the BNF Converter (bnfc 2.9.5).

-- | Program to test parser.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile, getContents)
import Data.Text.IO   ( getContents, readFile )
import qualified Data.Text.IO as TIO
import Data.Text qualified as T
import qualified Data.Text
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import Tiny.Syntax.AbsSyntax   (Statement)
import Tiny.Syntax.LexSyntax   ( Token, mkPosToken )
import Tiny.Syntax.ParSyntax   ( pStatement, myLexer )
import Tiny.Semant (transStatement, defaultStates, printStates)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Statement -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Statement -> Data.Text.Text -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      showTree v tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: Int -> Statement -> IO ()
showTree _ tree = do
  -- putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  -- putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree
  let res = transStatement defaultStates tree
  putStrLn "Searching possible states"
  case res of
    Left err -> do
      TIO.putStrLn $ "Error ocurred: " <> err
    Right states -> printStates states

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pStatement
    "-s":fs    -> mapM_ (runFile 0 pStatement) fs
    fs         -> mapM_ (runFile 2 pStatement) fs
