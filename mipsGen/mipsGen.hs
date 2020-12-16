import Control.Monad.Writer

data Expr
  = Val Int
  | Reg String
  | Var String

data Stmt
  = MACRO String
  | Define Expr   -- define a variable
  | Assign Expr Expr
  | Inc Expr
  | ForTime Int Stmt
  | For Expr Int Int Stmt   -- For (Var _) st ed Block
  | Block [Stmt]

type Addr = Int -- memory address

{- the environment of compiling progress -}
data Env = Env
  { labelId :: Int, -- for generate loop labels
    symList :: [(String, Addr)], -- symbol list
    heapTop :: Addr -- heap top address, for allocating
  }

-- add a new variable to an environment
newVariable :: Env -> Expr -> Env
newVariable (Env l s h) (Var v) = Env l ((v, h) : s) (h + 4)
newVariable _ _ = error "wrong type"

-- empty environment
env0 :: Env
env0 = Env 0 [] 0

(?=) :: Expr -> Expr -> Stmt
(?=) = Assign

eval :: Expr -> String
eval (Val n) = show n
eval _ = ""

-- yields the result and a new environment
compile :: Env -> Stmt -> (String, Env)
compile env (MACRO s) = (s, env)
{- it changes the compiler state, with no result -}
compile env (Define v) = ("", newVariable env v) 
compile env (Assign (Reg s) e) = ("addi " ++ s ++ ", $zero, " ++ eval e ++ "\n", env)
compile env (Inc (Reg s)) = ("addi " ++ s ++ ", " ++ s ++ ", 1" ++ "\n", env)

compile env (Block []) = ("", env)
compile env (Block (x:xs)) = 
  let (s, env') = compile env x in  -- compile the first stmt and yield a new env
    let (ss, envs) = compile env' (Block xs) in
      (s++ss, envs)
--compile env (Assign (Var))
-- compile :: Stmt -> String
-- compile (MACRO s) = s
-- compile (Assign (Reg s) e) = "addi " ++ s ++ ", $zero, " ++ eval e ++ "\n"
-- compile (Inc (Reg s)) = "addi " ++ s ++ ", " ++ s ++ ", 1" ++ "\n"
-- compile (ForTime n s) =
--   compile (Block [Reg "$t0" ?= Val 1, Reg "$t1" ?= Val n])
--     ++ "loop:\n"
--     ++ compile s
--     ++ compile (Block [Inc (Reg "$t0")])
-- compile (Block b) = concatMap compile b
-- compile (Assign _ _) = error "cannot assign to a right value"
-- compile (Inc _) = error "cannot assign to a right value"

main :: IO ()
main = do
  putStr s
  print $ symList env
  return ()
  where 
    (s, env) = compile env0 $ Block [
      Reg "t0" ?= Val 1, 
      Reg "t1" ?= Val 1,
      Define $ Var "a",
      Define $ Var "b",
      Block [Define $ Var "x"]
      ]
  