module Main where

import Lexer
import qualified Parser (parser)
import TypeChecker
import Interpreter
import System.IO (putStrLn)
import qualified Data.Map as Map
import Data.Maybe (isNothing)

-- Estado da avaliação do interpretador
type EvalState = (Expr, Store, Loc)

-- Executa a avaliação completa, aplicando stepState até chegar em um valor
runEvalState :: Expr -> EvalState
runEvalState initialExpr = go (initialExpr, Map.empty, 0)
  where
    go :: EvalState -> EvalState
    go state@(expr, _, _)
        | isValue expr = state
        | otherwise    = go (Interpreter.stepState state)

-- Executa parser → typecheck → interpretação
runBatch :: Expr -> String
runBatch ast =
    let sigma = Map.empty
        check = typecheck ast sigma
    in
        if isNothing check
        then "Type Error: Program is ill-typed.\n"
        else 
            let (finalExpr, finalStore, _) = runEvalState ast
            in  "AST Final (Resultado): " ++ show finalExpr ++ "\n"
             ++ "Store Final (Memória): " ++ show finalStore ++ "\n"

main :: IO ()
main = do
    putStrLn "Digite o programa na mesma linha:"
    input <- getLine
    let ast = Parser.parser (lexer input)
    putStrLn (runBatch ast)
