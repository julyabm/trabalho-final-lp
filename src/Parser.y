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
%left app

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
    'if'            { TokenIf }
    var             { TokenVar $$}
    '='             { TokenEq } 
    
    'lam'           { TokenLam }
    '->'            { TokenArrow }
    'nil'           { TokenNil }
    'listcons'      { TokenListCons }
    'isnil'         { TokenIsNil }
    'head'          { TokenHead }
    'tail'          { TokenTail }
    '['             { TokenLSquare }
    ']'             { TokenRSquare }

    'numeric'       { TokenKWNum }
    'bool'          { TokenKWBool }
%% 

Exp     : num                                 { Num $1 }
        | true                                { BTrue }
        | false                               { BFalse }
        | Exp '+' Exp                         { Add $1 $3 }
        | Exp '*' Exp                         { Times $1 $3 }
        | Exp "&&" Exp                        { And $1 $3 }
        | Exp "||" Exp                        { Or $1 $3 }
        | '(' Exp ')'                         { Paren $2 }
        | 'if' Exp Exp Exp                    { If $2 $3 $4 } 
        | var                                 { Var $1 }    
        | Exp Exp %prec app                   { App $1 $2 }
        | 'lam' var Ty '->' Exp               { Lam $2 $3 $5 }  

        | 'nil'      '[' Ty ']'               { Nil $3 }
        | 'listcons' '[' Ty ']' Exp Exp       { ListCons $3 $5 $6 }
        | 'isnil'    '[' Ty ']' Exp           { IsNil $3 $5 }      
        | 'head'     '[' Ty ']' Exp           { Head $3 $5 }        
        | 'tail'     '[' Ty ']' Exp           { Tail $3 $5 }


Ty  : 'numeric' { TNum }
    | 'bool'    { TBool }
    | Ty '->' Ty  { TFun $1 $3 } 
    | '[' Ty ']'  { TList $2 }


{ 

parseError :: [Token] -> a 
parseError _ = error "Syntax error!"

}