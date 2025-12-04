module Interpreter where 

import Lexer 
import Parser 

import qualified Data.Map as Map

type Env = Map.Map String Loc

isValue :: Expr -> Bool 
isValue BTrue  = True 
isValue BFalse = True 
isValue (Num _) = True 
isValue (Lam _ _ _) = True 
isValue (Loc _) = True
isValue Unit = True
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
subst x s (Or t1 t2) = Or (subst x s t1) (subst x s t2)
subst x s (If t1 t2 t3) = If (subst x s t1) (subst x s t2) (subst x s t3)
subst x s (Times t1 t2) = Times (subst x s t1) (subst x s t2)
subst x s (Paren t) = Paren (subst x s t)
subst x s (Ref t) = Ref (subst x s t)
subst x s (Deref t) = Deref (subst x s t)
subst x s (Assign t1 t2) = Assign (subst x s t1) (subst x s t2)
subst x s Unit = Unit
subst x s (Loc l) = Loc l
subst x s (Let y t1 t2) | x == y = Let y (subst x s t1) t2
subst x s (Let y t1 t2) | x /= y = Let y (subst x s t1) (subst x s t2)
subst _ _ e = e

-- step :: Expr -> Expr 
-- step (Add (Num n1) (Num n2)) = Num (n1 + n2)
-- step (Add (Num n1) e2) = let e2' = step e2
--                            in Add (Num n1) e2' 
-- step (Add e1 e2) = Add (step e1) e2 

-- step (Times (Num n1) (Num n2)) = Num (n1 * n2)
-- step (Times (Num n1) e2) = let e2' = step e2
--                             in Times (Num n1) e2' 
-- step (Times e1 e2) = Times (step e1) e2

-- step (And BFalse e2) = BFalse 
-- step (And BTrue e2) = e2 
-- step (And e1 e2) = And (step e1) e2 


-- step (Or BTrue e2) = BTrue
-- step (Or BFalse e2) = e2  
-- step (Or e1 e2) = Or (step e1) e2


-- step (If BTrue e1 e2) = e1
-- step (If BFalse e1 e2) = e2
-- step (If e e1 e2) = If (step e) e1 e2

-- step (App (Lam x tp e1) e2) = if (isValue e2) then 
--                                 subst x e2 e1 
--                               else 
--                                 App (Lam x tp e1) (step e2)



stepState :: (Expr, Store, Int) -> (Expr, Store, Int)

--E-APPABS
stepState (App (Lam x _ body) v, store, lcount) | isValue v = (subst x v body, store, lcount)

--(E-App1)
stepState (App t1 t2, store, lcount) | not (isValue t1) = let (t1', store', lcount') = stepState (t1, store, lcount)
                                 in (App t1' t2, store', lcount')

--(E-App2)
stepState (App v1 t2, store, lcount) | isValue v1 = let (t2', store', lcount') = stepState (t2, store, lcount)
                                 in (App v1 t2', store', lcount')

--E-DEREF
stepState (Deref t1, store, lcount) | not (isValue t1) = let (t1', store', lcount') = stepState (t1, store, lcount)
                                   in (Deref t1', store', lcount')

--E-DEREFLOC
stepState (Deref (Loc l), store, lcount) = case Map.lookup l store of 
                                        Just v  -> (v, store, lcount)
                                        Nothing -> error "Invalid memory access!"

--E-ASSIGN1
stepState (Assign t1 t2, store, lcount) | not (isValue t1) = let (t1', store', lcount') = stepState (t1, store, lcount)
                                      in (Assign t1' t2, store', lcount')

--E-ASSIGN2
stepState (Assign v1 t2, store, lcount) | isValue v1 && not (isValue t2) = let (t2', store', lcount') = stepState (t2, store, lcount)
                                      in (Assign v1 t2', store', lcount')

--E-ASSIGN
stepState (Assign (Loc l) v2, store, lcount) | isValue v2 = let store' = Map.insert l v2 store
                                                        in (Unit, store', lcount)

--E-REV
stepState (Ref t1, store, lcount) | not (isValue t1) = let (t1, store', lcount') = stepState (t1, store, lcount)
                                in (Ref t1, store', lcount')

--E-REFV
stepState (Ref v1, store, lcount) | isValue v1 = 
                                  let l = lcount 
                                      store' = Map.insert l v1 store
                                 in (Loc l, store', lcount + 1)

--E-Let
stepState (Let x t1 t2, store, lcount) | not (isValue t1) = 
                                 let (t1', store', lcount') = stepState (t1, store, lcount)
                                 in (Let x t1' t2, store', lcount')

--E-LetV (Se t1 é um valor v1)
stepState (Let x v1 t2, store, lcount) | isValue v1 = (subst x v1 t2, store, lcount)

-- Avaliação completa usando store e stepState
eval :: Expr -> Expr
eval e =
    let (v, _, _) = evalState (e, Map.empty, 0)
    in v


evalState :: (Expr, Store, Int) -> (Expr, Store, Int)
evalState (e, store, lcount)
    | isValue e = (e, store, lcount)
    | otherwise =
        let (e', store', lcount') = stepState (e, store, lcount)
        in evalState (e', store', lcount')
