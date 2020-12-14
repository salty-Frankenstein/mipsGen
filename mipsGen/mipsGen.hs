import Control.Monad.Writer

data Expr
  = Val Int
  | Reg String

data Stmt
  = MACRO String
  | Assign Expr Expr
  | Inc Expr
  | ForTime Int Stmt
  | Block [Stmt]

type Addr = Int -- memory address
{- the environment of  -}

data Env = Env
  { depth :: Int,
    vars :: [(String, Addr)]
  }

env0 = Env 0 []

(?=) :: Expr -> Expr -> Stmt
(?=) = Assign

eval :: Expr -> String
eval (Val n) = show n
eval _ = ""

compile :: Stmt -> String
compile (MACRO s) = s
compile (Assign (Reg s) e) = "addi " ++ s ++ ", $zero, " ++ eval e ++ "\n"
compile (Inc (Reg s)) = "addi " ++ s ++ ", " ++ s ++ ", 1" ++ "\n"
compile (ForTime n s) =
  compile (Block [Reg "$t0" ?= Val 1, Reg "$t1" ?= Val n])
    ++ "loop:\n"
    ++ compile s
    ++ compile (Block [Inc (Reg "$t0")])
compile (Block b) = concatMap compile b
compile (Assign _ _) = error "cannot assign to a right value"
compile (Inc _) = error "cannot assign to a right value"

main :: IO ()
main = do
  return ()