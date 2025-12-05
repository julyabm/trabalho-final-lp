module Interpreter where 

import Lexer 
import Parser 

isValue :: Expr -> Bool 
isValue BTrue  = True 
isValue BFalse = True 
isValue (Num _) = True 
isValue (Lam _ _ _) = True 

isValue (Nil _) = True
isValue (ListCons _ v1 v2) = isValue v1 && isValue v2

isValue _ = False 


subst :: String -> Expr -> Expr -> Expr 
subst x s y@(Var v) = if x == v then 
                        s 
                      else 
                        y 
subst x s (Num n) = (Num n)
subst x s BTrue = BTrue 
subst x s BFalse = BFalse 
subst x s (Lam y tp t1) = Lam y tp (subst x s t1)
subst x s (App t1 t2) = App (subst x s t1) (subst x s t2) 
subst x s (Add t1 t2) = Add (subst x s t1) (subst x s t2) 
subst x s (And t1 t2) = And (subst x s t1) (subst x s t2) 

-- Completar subst para outros termos da linguagem
subst x s (Or t1 t2) = Or (subst x s t1) (subst x s t2)
subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3)
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2)
subst x s (Paren t) = Paren (subst x s t)
--

subst x s (Nil t) = Nil t
subst x s (Head t e) = Head t (subst x s e)
subst x s (Tail t e) = Tail t (subst x s e)
subst x s (IsNil t e) = IsNil t (subst x s e)
subst x s (ListCons t e1 e2) = ListCons t (subst x s e1) (subst x s e2)


step :: Expr -> Expr 
step (App e1 e2) = App (step e1) e2

step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n1) e2) = let e2' = step e2
                           in Add (Num n1) e2' 
step (Add e1 e2) = Add (step e1) e2 

-- Implementar step para Times
step (Times (Num n1) (Num n2)) = Num (n1 * n2)
step (Times (Num n1) e2) = let e2' = step e2
                            in Times (Num n1) e2' 
step (Times e1 e2) = Times (step e1) e2
--

step (And BFalse e2) = BFalse 
step (And BTrue e2) = e2 
step (And e1 e2) = And (step e1) e2 

-- Implementar step para Or
step (Or BTrue e2) = BTrue
step (Or BFalse e2) = e2  
step (Or e1 e2) = Or (step e1) e2
--

-- Implementar step para If
step (If BTrue e1 e2) = e1
step (If BFalse e1 e2) = e2
step (If e e1 e2) = If (step e) e1 e2
--

step (App (Lam x tp e1) e2) = if (isValue e2) then 
                                subst x e2 e1 
                              else 
                                App (Lam x tp e1) (step e2)


step (ListCons t e1 e2) | not (isValue e1) = ListCons t (step e1) e2               --E-CONS1           
step (ListCons t v1 e2) | isValue v1 && not (isValue e2) = ListCons t v1 (step e2) --E-CONS2  
step (IsNil _ (Nil _)) = BTrue                                                     --E-ISNILNIL
step (IsNil _ (ListCons _ v1 v2)) = BFalse                                         --E-ISNILCONS
step (IsNil t e) = IsNil t (step e)                                                --E-ISNIL
step (Head _ (ListCons _ v1 v2)) = v1                                              --E-HEADCONS
step (Head t e) = Head t (step e)                                                  --E-HEAD
step (Tail _ (ListCons _ v1 v2)) = v2                                              --E-TAILCONS
step (Tail t e) = Tail t (step e)                                                  --E-TAIL


eval :: Expr -> Expr
eval e = if isValue e then 
           e
         else 
           eval (step e)
