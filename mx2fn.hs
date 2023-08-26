import JavaScript
import MathExpr
import Rust
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  contents <- getContents
  putStrLn (convert args (parse contents))

convert :: [String] -> Expr -> String
convert args =
  case args of
    ["--ts", fn] -> convertToTS fn
    ["--rs", fn] -> convertToRs fn
    ["--js", fn] -> convertToJS fn
    _ -> const "Usage: mathexpr [--ts|--rs|--js] <function name>"
