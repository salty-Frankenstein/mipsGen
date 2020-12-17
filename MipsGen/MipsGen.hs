module MipsGen.MipsGen where

import Control.Monad.State

debug = False

stAddr = if debug then 0x10010000 else 0

baseReg = "t9" -- register for heap base address

evalTmpReg = "t0"

cmpReg1 = "t1"

cmpReg2 = "t2"

initCode = "\taddi $" ++ baseReg ++ ", $zero, " ++ show stAddr ++ "\n"

data Expr
  = Val Int
  | Reg String
  | Var String
  | Lt Expr Expr -- less than

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
type LabelID = Int  -- for generating loop labels
type Code = String  -- the code generated

{- the environment of compiling progress -}
data Env = Env
  { symList :: [(String, Addr)], -- symbol list
    heapTop :: Addr -- heap top address, for allocating
  }

{- the compiler state -}
type CompileSt = (Env, LabelID, Code) 

{- the compiler monad -}
{- the compiling environment should be passd by a state monad -}
type CompileM = State CompileSt

incLabel :: CompileM ()
incLabel = state $ \(e, l, c) -> ((), (e, l + 1, c))

-- add a new variable to an environment
newVariable :: Expr -> CompileM ()
newVariable (Var v) = do
  (Env s h, l, c) <- get
  case lookup v s of
    Nothing -> put (Env ((v, h) : s) (h + 4), l, c)
    Just _ -> error $ "redefinition of symbol: " ++ v
newVariable _ = error "wrong type"

appendCode :: String -> CompileM ()
appendCode c' = state $ \(e, l, c) -> ((), (e, l, c ++ c'))

getEnv :: CompileM Env
getEnv = do
  (e, _, _) <- get
  return e

putEnv :: Env -> CompileM ()
putEnv e' = state $ \(_, l, c) -> ((), (e', l, c))

-- empty environment

env0 :: Env
env0 = Env [] stAddr

compileSt0 :: CompileSt
compileSt0 = (env0, 0, "")

(?=) :: Expr -> Expr -> Stmt
(?=) = Assign

(?<) :: Expr -> Expr -> Expr
(?<) = Lt

-- evaluate the expression in an environment
-- the result should be loaded to $t0
eval :: Env -> Expr -> String
eval _ (Val n) = show n
eval env (Var v) =
  case lookup v (symList env) of
    Nothing -> error $ "Undefined variables: " ++ v
    -- load variable to tmpreg 0
    Just x -> "\tlw $" ++ evalTmpReg ++ ", " ++ show x ++ "($" ++ baseReg ++ ")\n"
eval _ (Lt (Reg a) (Reg b)) =
  "\tslt $" ++ evalTmpReg ++ ", " ++ "$" ++ a ++ ", $" ++ b ++ "\n"
eval _ (Lt (Reg a) (Val n)) =
  "\tslti $" ++ evalTmpReg ++ ", " ++ "$" ++ a ++ ", " ++ show n ++ "\n"
eval env (Lt v@(Var _) a@(Reg _)) =
  eval env v ++ eval env (Reg evalTmpReg ?< a) 

eval _ _ = error "Unknown pattern"

-- yields the result and passing a new environment
compile :: Stmt -> CompileM ()
compile (MACRO s) = appendCode s
{- it changes the compiler state, with no result -}
compile (Define v) = newVariable v
{- assign register with imm -}
compile (Assign (Reg s) e@(Val _)) =
  do
    env <- getEnv
    appendCode $ "\taddi " ++ s ++ ", $zero, " ++ eval env e ++ "\n"
{- assign variables with regs -}
compile (Assign (Var v) (Reg r)) =
  do
    env <- getEnv
    case lookup v (symList env) of
      Nothing -> error $ "Undefined variables: " ++ v
      {- store into memory -}
      Just x -> appendCode $ "\tsw $" ++ r ++ ", " ++ show x ++ "($" ++ baseReg ++ ")\n"
{- assign variables with imm -}
compile (Assign v@(Var _) e@(Val _)) =
  do
    compile (Reg evalTmpReg ?= e)
    compile (v ?= Reg evalTmpReg)
compile (Inc (Reg s)) =
  appendCode $ "\taddi " ++ s ++ ", " ++ s ++ ", 1" ++ "\n"
compile (Inc (Var v)) =
  do
    env <- getEnv
    appendCode $ eval env (Var v)
    compile (Inc (Reg evalTmpReg))
    compile (Var v ?= Reg evalTmpReg)


compile (IF (Val _) _ _) = error "syntax error in if-statement"
compile (IF cond thenStmt elseStmt) =
  do
    (env, curLabel, _) <- get
    appendCode $ eval env cond -- condition expr, result in $t0
    let lID = show curLabel
        falseL = "false_label" ++ lID
        doneL = "done_label" ++ lID
    appendCode $ "\tblez $" ++ evalTmpReg ++ " " ++ falseL ++ "\n" -- if !cond goto false_label
    incLabel  -- update label
    compile thenStmt
    appendCode $ falseL ++ ":\n"  -- false label
    compile elseStmt
    appendCode $ doneL ++ ":\n" -- done label  


-- compile env (For (Var v) st ed s) =
--   let label = "loop" ++ show (labelId env)
--       code =
--         Block [
--           Define $ Var ("_" ++ label),
--         ]

compile (Block []) = return () -- do nothing
compile (Block (x : xs)) =
  do
    (oldEnv, _, _) <- get -- saving the old environment
    compile x
    compile (Block xs) -- complie the whole block with the environment changed
    putEnv oldEnv -- set the original env for scoping(??)

compile NOP = appendCode "\tnop\n"
{- errors -}
compile (Assign _ _) = error "cannot assign to a right value"
compile (Inc _) = error "cannot assign to a right value"
compile For {} = error "syntax error in for-statement"

runCompile :: Stmt -> String
runCompile s =
  let (_, _, c) = execState (compile s) compileSt0
   in initCode ++ c
