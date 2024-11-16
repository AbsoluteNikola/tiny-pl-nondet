-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.5).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Tiny.Syntax.ParSyntax
  ( happyError
  , myLexer
  , pStatement
  ) where

import Prelude

import qualified Tiny.Syntax.AbsSyntax
import Tiny.Syntax.LexSyntax
import qualified Data.Text

}

%name pStatement Statement
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'        { PT _ (TS _ 1)        }
  '%'        { PT _ (TS _ 2)        }
  '&&'       { PT _ (TS _ 3)        }
  '('        { PT _ (TS _ 4)        }
  ')'        { PT _ (TS _ 5)        }
  '*'        { PT _ (TS _ 6)        }
  '+'        { PT _ (TS _ 7)        }
  '-'        { PT _ (TS _ 8)        }
  '/'        { PT _ (TS _ 9)        }
  '/='       { PT _ (TS _ 10)       }
  ':='       { PT _ (TS _ 11)       }
  ';'        { PT _ (TS _ 12)       }
  '<'        { PT _ (TS _ 13)       }
  '<='       { PT _ (TS _ 14)       }
  '='        { PT _ (TS _ 15)       }
  '>'        { PT _ (TS _ 16)       }
  '>='       { PT _ (TS _ 17)       }
  '?'        { PT _ (TS _ 18)       }
  'U'        { PT _ (TS _ 19)       }
  '||'       { PT _ (TS _ 20)       }
  L_integ    { PT _ (TI $$)         }
  L_VarIdent { PT _ (T_VarIdent $$) }

%%

Integer :: { Integer }
Integer  : L_integ  { (read (Data.Text.unpack $1)) :: Integer }

VarIdent :: { Tiny.Syntax.AbsSyntax.VarIdent }
VarIdent  : L_VarIdent { Tiny.Syntax.AbsSyntax.VarIdent $1 }

Expr :: { Tiny.Syntax.AbsSyntax.Expr }
Expr
  : VarIdent { Tiny.Syntax.AbsSyntax.ExprVar $1 }
  | Integer { Tiny.Syntax.AbsSyntax.ExprConst $1 }
  | '(' Expr IntOp Expr ')' { Tiny.Syntax.AbsSyntax.ExprOp $2 $3 $4 }

IntOp :: { Tiny.Syntax.AbsSyntax.IntOp }
IntOp
  : '+' { Tiny.Syntax.AbsSyntax.Plus }
  | '-' { Tiny.Syntax.AbsSyntax.Minus }
  | '*' { Tiny.Syntax.AbsSyntax.Multiply }
  | '/' { Tiny.Syntax.AbsSyntax.Div }
  | '%' { Tiny.Syntax.AbsSyntax.Mod }

IntCondOp :: { Tiny.Syntax.AbsSyntax.IntCondOp }
IntCondOp
  : '=' { Tiny.Syntax.AbsSyntax.Eq }
  | '/=' { Tiny.Syntax.AbsSyntax.NotEq }
  | '>' { Tiny.Syntax.AbsSyntax.Gt }
  | '>=' { Tiny.Syntax.AbsSyntax.GtEq }
  | '<' { Tiny.Syntax.AbsSyntax.Lt }
  | '<=' { Tiny.Syntax.AbsSyntax.LtEq }

BoolCondOp :: { Tiny.Syntax.AbsSyntax.BoolCondOp }
BoolCondOp
  : '||' { Tiny.Syntax.AbsSyntax.Or }
  | '&&' { Tiny.Syntax.AbsSyntax.And }

Cond :: { Tiny.Syntax.AbsSyntax.Cond }
Cond
  : '(' Expr IntCondOp Expr ')' { Tiny.Syntax.AbsSyntax.IntCond $2 $3 $4 }
  | '(' Cond BoolCondOp Cond ')' { Tiny.Syntax.AbsSyntax.BoolCond $2 $3 $4 }
  | '(' '!' Cond ')' { Tiny.Syntax.AbsSyntax.NotCond $3 }

ListStatement :: { [Tiny.Syntax.AbsSyntax.Statement] }
ListStatement
  : Statement { (:[]) $1 }
  | Statement ';' ListStatement { (:) $1 $3 }

Statement :: { Tiny.Syntax.AbsSyntax.Statement }
Statement
  : VarIdent ':=' Expr { Tiny.Syntax.AbsSyntax.Assign $1 $3 }
  | Cond '?' { Tiny.Syntax.AbsSyntax.Test $1 }
  | '(' ListStatement ')' { Tiny.Syntax.AbsSyntax.Composition $2 }
  | '(' Statement 'U' Statement ')' { Tiny.Syntax.AbsSyntax.Union $2 $4 }
  | Statement '*' { Tiny.Syntax.AbsSyntax.Closure $1 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: Data.Text.Text -> [Token]
myLexer = tokens

}

