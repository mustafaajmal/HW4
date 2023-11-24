{-# LANGUAGE OverloadedStrings #-}

module Language.Nano.Eval
  ( execFile, execString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import Control.Exception (throw, catch)
import Language.Nano.Types
import Language.Nano.Parser

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Value
--------------------------------------------------------------------------------
execFile f = (readFile f >>= execString) `catch` exitError

--------------------------------------------------------------------------------
execString :: String -> IO Value
--------------------------------------------------------------------------------
execString s = execExpr (parseExpr s) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expr -> IO Value
--------------------------------------------------------------------------------
execExpr e = return (eval (prelude ++ env0) e) `catch` exitError

--------------------------------------------------------------------------------
parse :: String -> Expr
--------------------------------------------------------------------------------
parse = parseExpr

exitError :: Error -> IO Value
exitError (Error msg) = return (VErr msg)

eval :: Env -> Expr -> Value
eval env expr = case expr of
    ENil -> VNil
    EBool bool -> VBool bool
    EInt n -> value n
    EVar var -> lookupId var env
    EBin op lhs rhs -> evalOp op (eval env lhs) (eval env rhs)
    EIf cond thenExpr elseExpr -> 
        if eval env cond == VBool True then eval env thenExpr else eval env elseExpr
    ELet varName expr1 expr2 -> 
        let newEnv = (varName, eval newEnv expr1) : env
        in eval newEnv expr2
    ELam arg body -> VClos env arg body
    EApp funcExpr argExpr -> 
        let funcVal = eval env funcExpr
            argVal = eval env argExpr
        in case funcVal of
            VPrim primFunc -> primFunc argVal
            VClos closureEnv paramName body -> eval ((paramName, argVal) : closureEnv) body
            _ -> throw (Error "type error in EApp")

evalOp :: Binop -> Value -> Value -> Value
evalOp bop val1 val2 = case bop of
    Plus -> case (val1, val2) of
        (VInt x, VInt y) -> VInt (x + y)
        _ -> throw (Error "type error in evalOp Plus")
    Minus -> case (val1, val2) of
        (VInt x, VInt y) -> VInt (x - y)
        _ -> throw (Error "type error in evalOp Minus")
    Mul -> case (val1, val2) of
        (VInt x, VInt y) -> VInt (x * y)
        _ -> throw (Error "type error in evalOp Mul")
    Eq -> VBool (val1 == val2)
    Ne -> VBool (val1 /= val2)
    Lt -> case (val1, val2) of
        (VInt x, VInt y) -> VBool (x < y)
        _ -> throw (Error "type error in evalOp Lt")
    Le -> case (val1, val2) of
        (VInt x, VInt y) -> VBool (x <= y)
        _ -> throw (Error "type error in evalOp Le")
    And -> case (val1, val2) of
        (VBool x, VBool y) -> VBool (x && y)
        _ -> throw (Error "type error in evalOp And")
    Or -> case (val1, val2) of
        (VBool x, VBool y) -> VBool (x || y)
        _ -> throw (Error "type error in evalOp Or")
    Cons -> VPair val1 val2
    _ -> throw (Error "unsupported operation in evalOp")

lookupId :: Id -> Env -> Value
lookupId x [] = throw (Error ("unbound variable: " ++ x))
lookupId x ((y,v):ys)
  | x == y = v
  | otherwise = lookupId x ys

prelude :: Env
prelude =
  [ ("head", VPrim headFunc),
    ("tail", VPrim tailFunc)
  ]
  where
    headFunc (VPair a _) = a
    headFunc VNil = throw (Error "empty list")
    headFunc _ = throw (Error "type error in head")

    tailFunc (VPair _ b) = b
    tailFunc VNil = throw (Error "empty list")
    tailFunc _ = throw (Error "type error in tail")

env0 :: Env
env0 =  [ ("z1", VInt 0)
        , ("x" , VInt 1)
        , ("y" , VInt 2)
        , ("z" , VInt 3)
        , ("z1", VInt 4)
        ]
