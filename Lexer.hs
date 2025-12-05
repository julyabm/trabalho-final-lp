module Lexer where 

import Data.Char 

data Token = TokenNum Int 
           | TokenTrue 
           | TokenFalse
           | TokenPlus 
           | TokenTimes 
           | TokenAnd 
           | TokenOr 
           | TokenLParen 
           | TokenRParen 
           
           | TokenIf
           | TokenLam
           | TokenEq
           | TokenVar String

           | TokenNil
           | TokenListCons
           | TokenIsNil
           | TokenHead
           | TokenTail
           | TokenLSquare
           | TokenRSquare

           | TokenKWNum
           | TokenKWBool

           deriving Show 

data Expr = Num Int 
          | BTrue 
          | BFalse 
          | Add Expr Expr 
          | Times Expr Expr 
          | And Expr Expr 
          | Or Expr Expr 
          | Paren Expr 
          | If Expr Expr Expr 
          | Var String
          | Lam String Ty Expr 
          | App Expr Expr 

          | Nil Ty           
          | ListCons Ty Expr Expr
          | IsNil Ty Expr
          | Head Ty Expr
          | Tail Ty Expr

          deriving Show 

data Ty = TNum 
        | TBool 
        | TFun Ty Ty 
        | TList Ty
        deriving (Show, Eq) 

lexer :: String -> [Token]
lexer [] = []
lexer ('+':cs) = TokenPlus : lexer cs 
lexer ('*':cs) = TokenTimes : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('|':'|':cs) = TokenOr : lexer cs  
lexer ('=':cs) = TokenEq : lexer cs

lexer ('[':cs) = TokenLSquare : lexer cs 
lexer (']':cs) = TokenRSquare : lexer cs

lexer (c:cs) | isSpace c = lexer cs 
             | isDigit c = lexNum (c:cs)
             | isAlpha c = lexKw (c:cs)
lexer _ = error "Lexical error"

lexNum cs = case span isDigit cs of 
              (num, rest) -> TokenNum (read num) : lexer rest 

lexKw cs = case span isAlpha cs of 
             ("true", rest) -> TokenTrue : lexer rest 
             ("false", rest) -> TokenFalse : lexer rest
             ("if", rest) -> TokenIf : lexer rest 
            
             ("lam", rest) -> TokenLam : lexer rest
             ("=", rest) -> TokenEq : lexer rest

             ("nil", rest) -> TokenNil : lexer rest
             ("listcons", rest) -> TokenListCons : lexer rest
             ("isnil", rest) -> TokenIsNil : lexer rest
             ("head", rest) -> TokenHead : lexer rest
             ("tail", rest) -> TokenTail : lexer rest

             ("numeric", rest) -> TokenKWNum : lexer rest
             ("bool", rest) -> TokenKWBool : lexer rest

             (var, rest) -> TokenVar var: lexer rest
