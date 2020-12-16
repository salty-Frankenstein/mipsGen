import Control.Monad.Writer

debug = False

stAddr = if debug then 0x10010000 else 0

baseReg = "t9" -- register for heap base address

evalTmpReg = "t0"

cmpReg1 = "t1"
cmpReg2 = "t2"

initCode = "addi $" ++ baseReg ++ ", $zero, " ++ show stAddr ++ "\n"

data Expr
  = Val Int
  | Reg String
  | Var String
  | Lt Expr Expr  -- less than

data Stmt
  = MACRO String
  | Define Expr -- define a variable
  | Assign Expr Expr
  | Inc Expr
  | IF Expr Stmt Stmt
  | ForTime Int Stmt
  | For Expr Int Int Stmt -- For (Var _) st ed Block
  | Block [Stmt]
  | NOP

type Addr = Int -- memory address

{- the environment of compiling progress -}
data Env = Env
  { labelId :: Int, -- for generate loop labels
    symList :: [(String, Addr)], -- symbol list
    heapTop :: Addr -- heap top address, for allocating
  }

-- increase label id
incLabel :: Env -> Env
incLabel (Env l s h) = (Env (l+1) s h)

-- add a new variable to an environment
newVariable :: Env -> Expr -> Env
newVariable (Env l s h) (Var v) = 
  case lookup v s of
    Nothing -> Env l ((v, h) : s) (h + 4)
    Just _ -> error $ "redefinition of symbol: " ++ v
newVariable _ _ = error "wrong type"

-- empty environment
env0 :: Env
env0 = Env 0 [] stAddr

(?=) :: Expr -> Expr -> Stmt
(?=) = Assign

(?<=) :: Expr -> Expr -> Expr
(?<=) = Lt

-- evaluate the expression in an environment
-- the result should be loaded to $t0
eval :: Env -> Expr -> String
eval _ (Val n) = show n
eval env (Var v) =
  case lookup v (symList env) of
    Nothing -> error "Undefined variables"
    -- load variable to tmpreg 0
    Just x -> "lw $" ++ evalTmpReg ++ ", " ++ show x ++ "($" ++ baseReg ++ ")\n"
eval _ (Lt (Reg a) (Reg b)) =
  "slt $" ++ evalTmpReg ++ ", " ++ "$" ++ a ++ ", $" ++ b ++ "\n"
eval _ (Lt (Reg a) (Val n)) =
  "slti $" ++ evalTmpReg ++ ", " ++ "$" ++ a ++ ", " ++ show n ++ "\n"
-- eval _ (Lt (Var v))

eval _ _ = error "Unknown pattern"

-- yields the result and a new environment
compile :: Env -> Stmt -> (String, Env)
compile env (MACRO s) = (s, env)
{- it changes the compiler state, with no result -}
compile env (Define v) = ("", newVariable env v)

{- assign register with imm -}
compile env (Assign (Reg s) e@(Val _)) =
  ("addi " ++ s ++ ", $zero, " ++ eval env e ++ "\n", env)
{- assign variables with regs -}
compile env (Assign (Var v) (Reg r)) =
  case lookup v (symList env) of
    Nothing -> error "Undefined variables"
    {- store into memory -}
    Just x -> ("sw $" ++ r ++ ", " ++ show x ++ "($" ++ baseReg ++ ")\n", env)
{- assign variables with imm -}
compile env (Assign v@(Var _) e@(Val _)) = 
  let (toReg, _) = compile env (Reg evalTmpReg ?= e)
      (toVar, _) = compile env (v ?= Reg evalTmpReg)
    in (toReg ++ toVar, env)

compile env (Inc (Reg s)) = ("addi " ++ s ++ ", " ++ s ++ ", 1" ++ "\n", env)
compile env (Inc (Var v)) =
  let load = eval env (Var v)
      (incTmp, _) = compile env (Inc (Reg evalTmpReg))
      (store, _) = compile env (Var v ?= Reg evalTmpReg)
   in (load ++ incTmp ++ store, env)

{-
compile env (IF (Val _) _ _) = error "syntax error in if-statement"
compile env (IF e s1 s2) = 
  let cond = eval e
      lID = show $ labelId env
      falseL = "false_label" ++ lID ++ ":\n"
      doneL = "done_label" ++ lID ++ ":\n"
      (thenStmt, _) = compile ()
-}

-- compile env (For (Var v) st ed s) = 
--   let label = "loop" ++ show (labelId env)
--       code = 
--         Block [
--           Define $ Var ("_" ++ label),
--         ]

compile env (Block []) = ("", env)
compile env (Block (x : xs)) =
  let (s, env') = compile env x -- compile the first stmt and yield a new env
   in let (ss, envs) = compile env' (Block xs)
       in (s ++ ss, env) -- it returns the original env for scoping(??)

compile env NOP = ("nop\n", env)

{- errors -}
compile _ (Assign _ _) = error "cannot assign to a right value"
compile _ (Inc _) = error "cannot assign to a right value"
compile _ For {} = error "syntax error in for-statement"
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
  putStr (initCode ++ s)
  print $ symList env
  return ()
  where
    (s, env) =
      compile env0 $
        Block
          [ Reg "t0" ?= Val 1,
            Inc $ Reg "t0",
            Define $ Var "a",
            Inc $ Var "a",
            Var "a" ?= Reg "t0",
            Define $ Var "b",
            Var "b" ?= Val 10,
            Block
              [ --Var "x" ?= Reg "t0",  -- error: undefined
                Define $ Var "x",
                Var "x" ?= Reg "t0"
              ],
              --Var "x" ?= Reg "t0" -- error: out of scope
            Var "b" ?= Val 20
          ]