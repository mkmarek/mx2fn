module MathExpr
  ( parse,
    collectVariableNames,
    Expr (..),
  )
where

import Data.Char (digitToInt, isAlpha, isDigit, isSpace)
import GHC.Float (int2Double)

data Expr
  = Num Double
  | Empty
  | Fun String Expr
  | Var String
  | Add [Expr]
  | Mul [Expr]
  | Pow Expr Expr
  | Neg Expr
  | Recip Expr
  deriving (Eq, Show)

parse :: String -> Expr
parse s =
  let (t, _) = parseExpr (filter (not . isSpace) s)
   in t

collectVariableNames :: [String] -> Expr -> [String]
collectVariableNames names expr =
  case expr of
    Num _ -> names
    Empty -> names
    Fun _ e -> collectVariableNames names e
    Var n -> n : names
    Neg e -> collectVariableNames names e
    Pow e1 e2 -> collectVariableNames (collectVariableNames names e2) e1
    Add exps -> foldl collectVariableNames names exps
    Mul exps -> foldl collectVariableNames names exps
    Recip e -> collectVariableNames names e

parseExpr :: String -> (Expr, String)
parseExpr = parseAdditionOp []

parseAdditionOp :: [Expr] -> String -> (Expr, String)
parseAdditionOp acc s =
  let (factor, rest) = parseMultiplicationOp [] s
   in case rest of
        '-' : rest' -> parseSubtractOps (factor : acc) rest'
        '+' : rest' -> parseAdditionOp (factor : acc) rest'
        _ -> case acc of
          [] -> (factor, rest)
          _ -> (Add (factor : acc), rest)

parseSubtractOps :: [Expr] -> String -> (Expr, String)
parseSubtractOps acc s =
  let (factor, rest) = parseMultiplicationOp [] s
   in case rest of
        '-' : rest' -> parseSubtractOps (Neg factor : acc) rest'
        '+' : rest' -> parseAdditionOp (Neg factor : acc) rest'
        _ -> case acc of
          [] -> (Neg factor, rest)
          _ -> (Add (Neg factor : acc), rest)

parseMultiplicationOp :: [Expr] -> String -> (Expr, String)
parseMultiplicationOp acc s =
  let (factor, rest) = parseExponentOp s
   in case rest of
        '*' : rest' -> parseMultiplicationOp (factor : acc) rest'
        '/' : rest' -> parseDivideOp (factor : acc) rest'
        _ -> case acc of
          [] -> (factor, rest)
          _ -> (Mul (factor : acc), rest)

parseDivideOp :: [Expr] -> String -> (Expr, String)
parseDivideOp acc s =
  let (factor, rest) = parseExponentOp s
   in case rest of
        '*' : rest' -> parseMultiplicationOp (Recip factor : acc) rest'
        '/' : rest' -> parseDivideOp (Recip factor : acc) rest'
        _ -> case acc of
          [] -> (Recip factor, rest)
          _ -> (Mul (Recip factor : acc), rest)

parseExponentOp :: String -> (Expr, String)
parseExponentOp s =
  let (factor, rest) = parseUnaryOps s
   in case rest of
        [] -> (factor, [])
        '^' : rest' ->
          let (term, rest'') = parseExponentOp rest'
           in (Pow factor term, rest'')
        _ -> (factor, rest)

parseUnaryOps :: String -> (Expr, String)
parseUnaryOps s =
  case s of
    '-' : rest ->
      let (term, rest') = parseUnaryOps rest
       in (Neg term, rest')
    _ -> parseFactor s

parseFactor :: String -> (Expr, String)
parseFactor s =
  case s of
    '(' : rest ->
      let (expr, rest') = parseExpr rest
       in case rest' of
            ')' : rest'' -> (expr, rest'')
            _ -> error "Missing closing parenthesis"
    firstChar : _ ->
      case firstChar of
        c | isDigit c -> parseNumber s
        c | isAlpha c -> parseVariableOrFunction s
        _ -> error ("Parse error: " ++ s)
    _ -> error "Parse error"

parseVariableOrFunction :: String -> (Expr, String)
parseVariableOrFunction s =
  let (name, rest) = span isVariableChar s
   in case rest of
        '(' : rest' ->
          let (args, rest'') = parseExpr rest'
           in case rest'' of
                ')' : rest''' -> (Fun name args, rest''')
                _ -> error "Missing closing parenthesis"
        _ -> (Var name, rest)

isVariableChar :: Char -> Bool
isVariableChar c = isAlpha c || isDigit c || c == '_'

parseNumber :: String -> (Expr, String)
parseNumber s =
  let (digits, rest) = span isDigit s
   in case rest of
        '.' : rest' ->
          let (digits', rest'') = span isDigit rest'
           in (Num (read (digits ++ "." ++ digits')), rest'')
        _ -> (Num (read digits), rest)
