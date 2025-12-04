module TypeChecker where 

import qualified Data.Map as Map

import Lexer 

type Ctx = [(String, Ty)]
type Sigma = Map.Map Loc Ty


typeof :: Ctx -> Sigma -> Expr -> Maybe Ty 
typeof ctx sigma BTrue = Just TBool 
typeof ctx sigma BFalse = Just TBool 
typeof ctx sigma (Paren e) = typeof ctx sigma e
typeof ctx sigma (Num n) = Just TNum 
typeof ctx sigma (Add e1 e2) = case (typeof ctx sigma e1, typeof ctx sigma e2) of 
                           (Just TNum, Just TNum) -> Just TNum 
                           _                      -> Nothing

typeof ctx sigma (Times e1 e2) = case (typeof ctx sigma e1, typeof ctx sigma e2) of 
                           (Just TNum, Just TNum) -> Just TNum 
                           _                      -> Nothing

typeof ctx sigma (And e1 e2) = case (typeof ctx sigma e1, typeof ctx sigma e2) of 
                           (Just TBool, Just TBool) -> Just TBool 
                           _                        -> Nothing

typeof ctx sigma (Or e1 e2) = case (typeof ctx sigma e1, typeof ctx sigma e2) of 
                           (Just TBool, Just TBool) -> Just TBool 
                           _                        -> Nothing

typeof ctx sigma (If e e1 e2) = case typeof ctx sigma e of 
                            Just TBool -> case (typeof ctx sigma e1, typeof ctx sigma e2) of 
                                            (Just t1, Just t2) | t1 == t2  -> Just t1 
                                                               | otherwise -> Nothing 
                                            _ -> Nothing  
                            _ -> Nothing 

typeof ctx sigma (Var x) = lookup x ctx 

typeof ctx sigma (Lam x tp b) = let ctx' = (x,tp) : ctx 
                            in case (typeof ctx' sigma b) of 
                                 Just tr -> Just (TFun tp tr)
                                 _ -> Nothing 

typeof ctx sigma (App e1 e2) = case typeof ctx sigma  e1 of 
                           Just (TFun tp tr) -> case typeof ctx sigma e2 of 
                                                  Just t2 | t2 == tp -> Just tr 
                                                  _ -> Nothing 
                           _ -> Nothing 


typeof ctx sigma (Let x e1 e2) = case typeof ctx sigma e1 of 
                                   Just t1 -> let ctx' = (x, t1) : ctx 
                                              in typeof ctx' sigma e2
                                   _       -> Nothing

typeof ctx sigma (Unit) = Just TUnit

--pag 159 do livro, especificação de tipos para referências
--T-REF
typeof ctx sigma (Ref e) = case typeof ctx sigma e of 
                       Just t  -> Just (TRef t)
                       _       -> Nothing

--T-DEREF
typeof ctx sigma (Deref e) = case typeof ctx sigma e of 
                         Just (TRef t) -> Just t 
                         _             -> Nothing

--T-ASSIGN
-- aqui t2 tem que ser do mesmo tipo que o referenciado por t1, por isso verificação de igualdade
typeof ctx sigma (Assign e1 e2) = case (typeof ctx sigma e1, typeof ctx sigma e2) of 
                              (Just (TRef t1), Just t2) | t1 == t2 -> Just TUnit
                              _                                    -> Nothing

--T-LOC
typeof ctx sigma (Loc l) = case Map.lookup l sigma of 
                             Just t  -> Just (TRef t) 
                             _       -> Nothing


typecheck :: Expr -> Sigma -> Maybe Ty
typecheck e sigma = typeof [] sigma e
