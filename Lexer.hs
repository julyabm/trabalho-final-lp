module Lexer where 

import Data.Char 
import qualified Data.Map as Map

--"We can think of the store as an array of values... we can abstract away from the fact that references are numbers. "
type Loc = Int
type Store = Map.Map Loc Expr

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
           | TokenUnit
           | TokenRef
           | TokenDeref
           | TokenAssign
           | TokenColon
           | TokenArrow
           | TokenLet 
           | TokenEq
           | TokenIn
           | TokenVar String
           | TokenSemicolon
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
          | Ref Expr
          | Deref Expr 
          | Assign Expr Expr
          | Unit
          | Loc Int
          | Let String Expr Expr
          deriving Show 

data Ty = TNum 
        | TBool 
        | TFun Ty Ty 
        | TRef Ty
        | TUnit
        deriving (Show, Eq) 

lexer :: String -> [Token]
lexer [] = []
lexer ('r':'e':'f':cs) = TokenRef : lexer cs
lexer ('!':cs) = TokenDeref : lexer cs
lexer (':':'=':cs) = TokenAssign : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs 
lexer ('*':cs) = TokenTimes : lexer cs 
lexer ('(':cs) = TokenLParen : lexer cs 
lexer (')':cs) = TokenRParen : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs 
lexer ('|':'|':cs) = TokenOr : lexer cs 
lexer (':':cs) = TokenColon : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
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
                ("unit", rest) -> TokenUnit : lexer rest
                ("lam", rest) -> TokenLam : lexer rest
                ("let", rest) -> TokenLet : lexer rest 
                ("in", rest) -> TokenIn : lexer rest
                (":", rest) -> TokenColon : lexer rest
                ("->", rest) -> TokenArrow : lexer rest
                ("ref", rest) -> TokenRef : lexer rest
                ("!", rest) -> TokenDeref : lexer rest
                (":=", rest) -> TokenAssign : lexer rest
                ("=", rest) -> TokenEq : lexer rest
                (var, rest) -> TokenVar var: lexer rest