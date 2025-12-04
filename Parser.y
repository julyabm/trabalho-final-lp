{
module Parser where 

import Lexer 
}

%name parser
%tokentype { Token }
%error { parseError }

%left "||"
%left "&&"
%left '+'
%left '*'
%right ':='
%right '!'
%right 'ref'


%token 
    num             { TokenNum $$ }
    true            { TokenTrue }
    false           { TokenFalse }
    '+'             { TokenPlus }
    '*'             { TokenTimes }
    "&&"            { TokenAnd }
    "||"            { TokenOr }
    '('             { TokenLParen }
    ')'             { TokenRParen }
    'ref'           { TokenRef }
    '!'             { TokenDeref }
    ':='            { TokenAssign }
    'if'            { TokenIf }
    'unit'          { TokenUnit }   
    'lam'           { TokenLam }
    'let'           { TokenLet } 
    '='             { TokenEq } 
    'in'            { TokenIn } 
    var             { TokenVar $$}
    ':'             { TokenColon }
    '->'            { TokenArrow }

%% 

Exp     : num           { Num $1 }
        | true          { BTrue }
        | false         { BFalse }
        | Exp '+' Exp   { Add $1 $3 }
        | Exp '*' Exp   { Times $1 $3 }
        | Exp "&&" Exp  { And $1 $3 }
        | Exp "||" Exp  { Or $1 $3 }
        | '(' Exp ')'   { Paren $2 }
        | 'ref' Exp     { Ref $2 }
        | '!' Exp       { Deref $2 }
        | Exp ':=' Exp  { Assign $1 $3 }
        | 'unit'        { Unit }
        | 'if' Exp Exp Exp  { If $2 $3 $4 } 
        | var           { Var $1 }
        | 'lam' var ':' Type '->' Exp  { Lam $2 $4 $6 } 
        | Exp Exp               { App $1 $2 }
        | 'let' var '=' Exp 'in' Exp  { Let $2 $4 $6 }

Type    : num                { TNum }
        | 'unit'             { TUnit }
        | Type '->' Type     { TFun $1 $3 }
        | 'ref' Type         { TRef $2 }


{ 

parseError :: [Token] -> a 
parseError _ = error "Syntax error!"

}