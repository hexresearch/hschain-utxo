{
module Hschain.Utxo.Lang.Parser.Lexer (
  Token(..),
  scanTokens
) where

import Control.Monad.Except

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]
$graphic    = $printable # $white

-- macro definitions
@exponent = (e | E) (\+ | \-)? $digit+ 
@fraction = \. $digit+
@int_part = $digit+
@point_float = (@int_part? @fraction) | @int_part \.
@exponent_float = (@int_part | @point_float) @exponent
@float_number = @point_float | @exponent_float
@int_number = (\-)? $digit+

$hexdig     = [0-9A-Fa-f]
@unicode    = \\ $hexdig{1,6} $white?
@escape		= @unicode | \\[^\r\n\f0-9a-f]
@lineterm   = [\n\r\f] | \r\n

@str1       = \" (@escape | \\ @lineterm | ~[\"])* \"
@str2       = '  (@escape | \\ @lineterm | ~['])*  '
@string     = @str1 | @str2

tokens :-

  -- Whitespace insensitive
  $eol                          ;
  $white+                       ;

  -- Comments
  "#".*                         ;

  -- Syntax
  let                           { \s -> TokenLet }
  map                           { \s -> TokenMap }
  fold                          { \s -> TokenFold }
  length                        { \s -> TokenLength }
  if                            { \s -> TokenIf }
  then                          { \s -> TokenThen }
  else                          { \s -> TokenElse }
  pk                            { \s -> TokenPk }
  True                          { \s -> TokenTrue }
  False                         { \s -> TokenFalse }
  in                            { \s -> TokenIn }
  @int_number                   { \s -> TokenNum (read s) }
  @float_number                 { \s -> TokenFloat (read s) }
  "->"                          { \s -> TokenArrow }
  "<"                           { \s -> TokenLessThan }
  ">"                           { \s -> TokenGreaterThan }
  "<="                          { \s -> TokenLessThanEquals }
  ">="                          { \s -> TokenGreaterThanEquals }
  "=="                          { \s -> TokenEquals }
  "/="                          { \s -> TokenNotEquals }
  "<>"                          { \s -> TokenConcatText }
  \=                            { \s -> TokenEq }
  \\                            { \s -> TokenLambda }
  [\+]{2}                       { \s -> TokenAppend }
  [\+]                          { \s -> TokenAdd }
  [\-]                          { \s -> TokenSub }
  [\*]                          { \s -> TokenMul }
  [\/]                          { \s -> TokenDiv }
  [\&]{2}                       { \s -> TokenAnd }
  [\|]{2}                       { \s -> TokenOr }
  [\:]{2}                       { \s -> TokenDColon }
  \'                            { \s -> TokenQuote }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \[                            { \s -> TokenLBracket }
  \]                            { \s -> TokenRBracket }
  \:                            { \s -> TokenColon }
  \.                            { \s -> TokenDot }
  \,                            { \s -> TokenComma }
  [\!]{2}                       { \s -> TokenDBang }
  \!                            { \s -> TokenBang }
  @string                       { \s -> TokenStringLiteral (init (tail s)) }

  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }

{

data Token 
  = TokenLet
  | TokenMap
  | TokenFold
  | TokenLength
  | TokenPk
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenTrue
  | TokenFalse
  | TokenIn
  | TokenLambda
  | TokenNum Int
  | TokenFloat Double
  | TokenStringLiteral String
  | TokenSym String
  | TokenArrow
  | TokenEq
  | TokenAdd
  | TokenSub
  | TokenMul
  | TokenDiv
  | TokenAnd
  | TokenOr
  | TokenLParen
  | TokenRParen
  | TokenLBracket
  | TokenRBracket
  | TokenQuote
  | TokenDot
  | TokenComma
  | TokenLessThan
  | TokenGreaterThan
  | TokenLessThanEquals
  | TokenGreaterThanEquals
  | TokenEquals
  | TokenNotEquals
  | TokenDColon
  | TokenColon
  | TokenBang
  | TokenAppend
  | TokenDBang
  | TokenConcatText
  | TokenEOF
  deriving (Eq,Show)

scanTokens :: String -> Except String [Token]
scanTokens str = go ('\n',[],str) where 
  go inp@(_,_bs,str) =
    case alexScan inp 0 of
     AlexEOF -> return []
     AlexError _ -> throwError "Invalid lexeme."
     AlexSkip  inp' len     -> go inp'
     AlexToken inp' len act -> do
      res <- go inp'
      let rest = act (take len str)
      return (rest : res)

}

