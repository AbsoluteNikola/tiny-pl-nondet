{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Tiny.Semant where

import Tiny.Syntax.AbsSyntax
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, pack, intercalate)
import qualified Data.Text.IO as TIO
import Prelude hiding (fail)
import GHC.Num (integerToInt)
import Data.Traversable (for)
import Control.Monad (join, when)
import Data.Bool (bool)
import Data.Foldable (for_)

newtype State = State { unState :: Map.Map Text Int }
  deriving (Eq, Ord, Show)

type States = Set.Set State

printStates :: States -> IO ()
printStates states = do
  when (Set.null states) $
    TIO.putStrLn "No possible states"
  for_ (zip [(1:: Int)..] (Set.toList states)) $ \(ix, State s) -> do
    let
      vars = map (\(name, value) -> name <> " = " <> (pack . show $ value))
        $ Map.toList s
      resultToPrint = (pack . show $ ix) <> ": " <> intercalate ", " vars
    TIO.putStrLn resultToPrint

defaultStates :: States
defaultStates = Set.singleton (State Map.empty)

type Result' a = Either Text a
type Result = Result' States

failureCase :: Show a => a -> Result' b
failureCase x = Left $ "Undefined case: " <> pack (show x)

fail :: Text -> Result' a
fail = Left

setVarInState :: State -> VarIdent -> Int -> State
setVarInState (State vars) (VarIdent varName) value =
  State $ Map.insert varName value vars

getVarFromState :: State -> VarIdent -> Result' Int
getVarFromState (State vars) (VarIdent varName) = case Map.lookup varName vars of
  Just x -> pure x
  Nothing -> fail $ "No var: " <> varName

transExpr :: State -> Expr -> Result' Int
transExpr s x = case x of
  ExprVar varident -> getVarFromState s varident
  ExprConst integer -> pure $ integerToInt integer
  ExprOp expr1 intop expr2 -> do
    i1 <- transExpr s expr1
    i2 <- transExpr s expr2
    pure $ transIntOp intop i1 i2

transIntOp :: IntOp -> Int -> Int -> Int
transIntOp x = case x of
  Plus -> (+)
  Minus -> (-)
  Multiply -> (+)
  Div -> div
  Mod -> mod


transIntCondOp :: IntCondOp -> Int -> Int -> Bool
transIntCondOp x = case x of
  Eq -> (==)
  NotEq -> (/=)
  Gt -> (>)
  GtEq -> (>=)
  Lt -> (<)
  LtEq -> (<=)

transBoolCondOp :: BoolCondOp -> Bool -> Bool -> Bool
transBoolCondOp x = case x of
  Or -> (||)
  And -> (&&)

transCond :: State -> Cond -> Result' Bool
transCond s x = case x of
  IntCond expr1 intcondop expr2 -> do
    i1 <- transExpr s expr1
    i2 <- transExpr s expr2
    pure $ transIntCondOp intcondop i1 i2
  BoolCond cond1 boolcondop cond2 -> do
    c1 <- transCond s cond1
    c2 <- transCond s cond2
    pure $ transBoolCondOp boolcondop c1 c2
  NotCond cond -> do
    c <- transCond s cond
    pure $ not c

traverseStates :: States -> (State -> Result' [State]) -> Result
traverseStates (Set.toList -> states) = fmap (Set.fromList . join) . for states

transStatement :: States -> Statement -> Result
transStatement states x = case x of
  Assign varident expr -> traverseStates states $ \s -> do
    i <- transExpr s expr
    pure [setVarInState s varident i]
  Test cond -> traverseStates states $ \s -> do
    isOk <- transCond s cond
    pure $ bool [] [s] isOk
  Composition statement1 statement2 -> do
    states1 <- transStatement states statement1
    transStatement states1 statement2
  Union statement1 statement2 -> do
    states1 <- transStatement states statement1
    states2 <- transStatement states statement2
    pure $ Set.union states1 states2
  Closure statement -> do
    newStates <- transStatement states statement
    if newStates == states
      then pure newStates
      else transStatement (Set.union newStates states) statement
