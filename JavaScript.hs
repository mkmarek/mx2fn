module JavaScript
  ( convertToJS,
    convertToTS,
  )
where

import Data.List (intercalate, nub, sort)
import MathExpr

convertToJS :: String -> Expr -> String
convertToJS fn expr =
  "function "
    ++ fn
    ++ " ("
    ++ intercalate ", " ((sort . nub . collectVariableNames []) expr)
    ++ ") {\n  return "
    ++ convertExpr 0 expr
    ++ ";\n}"

convertToTS :: String -> Expr -> String
convertToTS fn expr =
  "function "
    ++ fn
    ++ " ("
    ++ intercalate ", " (map (++ ": number") ((sort . nub . collectVariableNames []) expr))
    ++ "): number {\n  return "
    ++ convertExpr 0 expr
    ++ ";\n}"

convertExpr :: Word -> Expr -> String
convertExpr depth exp =
  case exp of
    Num n -> show n
    Empty -> ""
    Fun n e ->
      case n of
        "sin" -> "Math.sin(" ++ convertExpr 0 e ++ ")"
        "cos" -> "Math.cos(" ++ convertExpr 0 e ++ ")"
        "tan" -> "Math.tan(" ++ convertExpr 0 e ++ ")"
        "asin" -> "Math.asin(" ++ convertExpr 0 e ++ ")"
        "acos" -> "Math.acos(" ++ convertExpr 0 e ++ ")"
        "atan" -> "Math.atan(" ++ convertExpr 0 e ++ ")"
        "sinh" -> "Math.sinh(" ++ convertExpr 0 e ++ ")"
        "cosh" -> "Math.cosh(" ++ convertExpr 0 e ++ ")"
        "tanh" -> "Math.tanh(" ++ convertExpr 0 e ++ ")"
        _ -> n ++ "(" ++ convertExpr 0 e ++ ")"
    Var n -> n
    Neg e -> "-" ++ convertExpr (depth + 1) e
    Pow e1 e2 -> convertExpr (depth + 1) e1 ++ " ** " ++ convertExpr (depth + 1) e2
    Add exps ->
      if depth > 0
        then "(" ++ convertAdditions 0 exps ++ ")"
        else convertAdditions (depth + 1) exps
    Mul exps -> convertMultiplications (depth + 1) exps
    Recip e -> "(1 / " ++ convertExpr (depth + 1) e ++ ")"

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
