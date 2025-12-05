{
module Parser where 

import Lexer 
}

%name parser 
%tokentype { Token }
%error { parseError }

%left '+'
%left '*'

%left "||"
%left "&&"

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

Exp     : num              { Num $1 }
        | true             { BTrue }
        | false            { BFalse }
        | Exp '+' Exp      { Add $1 $3 }
        | Exp '*' Exp      { Times $1 $3 }
        | Exp "&&" Exp     { And $1 $3 }
        | Exp "||" Exp     { Or $1 $3 }
        | '(' Exp ')'      { Paren $2 }
        | 'if' Exp Exp Exp { If $2 $3 $4 } 
        | var              { Var $1 }      
        | Exp Exp          { App $1 $2 }

        | 'nil'      '[' BaseListTy ']'         { Nil $3 }
        | 'listcons' '[' BaseListTy ']' Exp Exp { ListCons $3 $5 $6 }
        | 'isnil'    '[' BaseListTy ']' Exp     { IsNil $3 $5 }      
        | 'head'     '[' BaseListTy ']' Exp     { Head $3 $5 }        
        | 'tail'     '[' BaseListTy ']' Exp     { Tail $3 $5 }


BaseListTy      : 'numeric'                 { TNum }
                | 'bool'                    { TBool }


{ 

parseError :: [Token] -> a 
parseError _ = error "Syntax error!"

}