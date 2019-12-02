{
{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Hschain.Utxo.Lang.Parser.Parser (
  parseExpr,
  parseTokens,
) where

import Data.Fix
import Data.String

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Parser.Lexer

import Control.Monad.Except

import qualified Data.Vector as V

}

-- Entry point
%name expr

-- Entry point
%name expr

-- Lexer structure 
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let    { TokenLet }
    map    { TokenMap }
    fold   { TokenFold }
    length { TokenLength }
    pk     { TokenPk }
    if     { TokenIf }
    then   { TokenThen }
    else   { TokenElse }
    true   { TokenTrue }
    false  { TokenFalse }
    in     { TokenIn }
    NUM    { TokenNum $$ }
    VAR    { TokenSym $$ }
    '\\'   { TokenLambda }
    '->'   { TokenArrow }
    STRING { TokenStringLiteral $$ } 
    DOUBLE { TokenFloat $$ }
    '='    { TokenEq }
    '+'    { TokenAdd }
    '-'    { TokenSub }
    '*'    { TokenMul }
    '/'    { TokenDiv }
    '&&'   { TokenAnd }
    '||'   { TokenOr }
    '('    { TokenLParen }
    ')'    { TokenRParen }
    '['    { TokenLBracket }
    ']'    { TokenRBracket }
    ','    { TokenComma }
    '\''   { TokenQuote }
    '.'    { TokenDot }
    '<'    { TokenLessThan }
    '>'    { TokenGreaterThan }
    '<='   { TokenLessThanEquals }
    '>='   { TokenGreaterThanEquals }
    '=='   { TokenEquals }
    '/='   { TokenNotEquals }
    '::'   { TokenDColon }
    ':'    { TokenColon }
    '!!'   { TokenDBang }
    '!'    { TokenBang }
    '++'   { TokenAppend }
    '<>'   { TokenConcatText }

-- Operators
%left '||' 
%left '&&'
%left '==' '/=' 
%left '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/'
%left '.'
%%

Expr : let VAR '=' Expr in Expr    { singleLet (fromString $2) $4 $6 }
     | let VAR LetArgs '=' Expr in Expr { Fix (LetArg (fromString $2) (reverse $ fmap fromString $3) $5 $7) }
     | '\\' LetArgs '->' Expr      { Fix (LamList (fmap fromString $ reverse $2) $4) }
     | pk Expr                     { Fix (Pk $2) } 
     | if Expr then Expr else Expr { Fix (If $2 $4 $6) }
     | Expr '!' Expr               { Fix (VecE (VecAt $1 $3)) }
     | Expr '++' Expr              { Fix (VecE (VecAppend $1 $3 )) }
     | '(' TupleExpr ')'           { Fix (Tuple (V.fromList (reverse $2))) }
     | Expr '!!' NUM               { Fix (UnOpE (TupleAt $3) $1) }
     | '(' Expr ')'                { $2 }
     | Form                        { $1 }

LetArgs : VAR                      { [$1] }
     | LetArgs VAR                 { $2 : $1 }   

VecExpr : Expr                     { [$1] }
        | VecExpr ',' Expr         { $3 : $1 }


TupleExpr : Expr                   { [$1] }
        | TupleExpr ',' Expr       { $3 : $1 }

Form : Form '+' Form               { Fix (BinOpE Plus  $1 $3) }
     | Form '-' Form               { Fix (BinOpE Minus $1 $3) }
     | Form '*' Form               { Fix (BinOpE Times $1 $3) }
     | Form '/' Form               { Fix (BinOpE Div   $1 $3) }
     | Form '==' Form              { Fix (BinOpE Equals $1 $3) }
     | Form '/=' Form              { Fix (BinOpE NotEquals $1 $3) }
     | Form '<' Form               { Fix (BinOpE LessThan $1 $3) }
     | Form '>' Form               { Fix (BinOpE GreaterThan $1 $3) }
     | Form '<=' Form              { Fix (BinOpE LessThanEquals $1 $3) }
     | Form '>=' Form              { Fix (BinOpE GreaterThanEquals $1 $3) }
     | Form '||' Form              { Fix (BinOpE Or $1 $3) }
     | Form '&&' Form              { Fix (BinOpE And $1 $3) }
     | Form '<>' Form              { Fix (TextE (TextAppend $1 $3)) }
     | Form '.'  Form              { Fix (BinOpE ComposeFun $1 $3) }
     | Fact                        { $1 }

Fact : Fact Atom                   { Fix (Apply $1 $2) }
     | Atom                        { $1 }

Atom : '(' Expr ')'                { $2 }
     | NUM                         { Fix (PrimE (PrimInt $1)) }
     | DOUBLE                      { Fix (PrimE (PrimDouble $1)) }
     | STRING                      { Fix (PrimE (PrimString (fromString $1))) }
     | map                         { Fix (VecE VecMap) }
     | fold                        { Fix (VecE VecFold) }
     | length                      { Fix (VecE VecLength) }
     | VAR                         { Fix (Var (fromString $1)) }
     | true                        { Fix (PrimE (PrimBool True)) }
     | false                       { Fix (PrimE (PrimBool False)) }
     | '[' VecExpr ']'             { Fix (VecE (NewVec (V.fromList (reverse $2)))) }
     | '[' ']'                     { Fix (VecE (NewVec mempty)) }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String Lang
parseExpr input = runExcept $ do
  tokenStream <- scanTokens input
  expr tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens
    
}
