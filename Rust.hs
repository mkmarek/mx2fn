module Rust
  ( convertToRs,
  )
where

import Data.List (intercalate, nub, sort)
import Distribution.Compat.ResponseFile (expandResponse)
import MathExpr

convertToRs :: String -> Expr -> String
convertToRs fn expr =
  "fn "
    ++ fn
    ++ " ("
    ++ intercalate ", " (map (++ ": f32") ((sort . nub . collectVariableNames []) expr))
    ++ ") -> f32 {\n  "
    ++ convertExpr 0 expr
    ++ "\n}"

convertExpr :: Word -> Expr -> String
convertExpr depth exp =
  case exp of
    Num n -> show n
    Empty -> ""
    Fun n e ->
      case n of
        "sin" -> wrapInParenthesis e ++ ".sin()"
        "cos" -> wrapInParenthesis e ++ ".cos()"
        "tan" -> wrapInParenthesis e ++ ".tan()"
        "asin" -> wrapInParenthesis e ++ ".asin()"
        "acos" -> wrapInParenthesis e ++ ".acos()"
        "atan" -> wrapInParenthesis e ++ ".atan()"
        "sinh" -> wrapInParenthesis e ++ ".sinh()"
        "cosh" -> wrapInParenthesis e ++ ".cosh()"
        "tanh" -> wrapInParenthesis e ++ ".tanh()"
        _ -> error "Unknown function: " ++ n
    Var n -> n
    Neg e -> "-" ++ convertExpr (depth + 1) e
    Pow e1 e2 -> wrapInParenthesis e1 ++ ".powf(" ++ convertExpr 0 e2 ++ ")"
    Add exps ->
      if depth > 0
        then "(" ++ convertAdditions 0 exps ++ ")"
        else convertAdditions (depth + 1) exps
    Mul exps -> convertMultiplications (depth + 1) exps
    Recip e -> "(1 / " ++ convertExpr (depth + 1) e ++ ")"

wrapInParenthesis :: Expr -> String
wrapInParenthesis expr =
  case expr of
    Var _ -> convertExpr 0 expr
    Num _ -> convertExpr 0 expr
    _ -> "(" ++ convertExpr 0 expr ++ ")"

convertAdditions :: Word -> [Expr] -> String
convertAdditions depth exps =
  case exps of
    [] -> ""
    [e] -> convertExpr depth e
    Neg e : es -> convertAdditions depth es ++ " - " ++ convertExpr depth e
    e : es -> convertAdditions depth es ++ " + " ++ convertExpr depth e

convertMultiplications :: Word -> [Expr] -> String
convertMultiplications depth exps =
  case exps of
    [] -> ""
    [e] -> convertExpr depth e
    Recip e : es -> convertMultiplications depth es ++ " / " ++ convertExpr depth e
    e : es -> convertMultiplications depth es ++ " * " ++ convertExpr depth e
