module MipsGen.MipsGen where

import Control.Monad.State

debug = True

stAddr = if debug then 0x10010000 else 0

baseReg = "t9" -- register for heap base address

evalTmpReg = "t0"
addrTmpReg = "t3"

cmpReg1 = "t1"

cmpReg2 = "t2"

initCode = "\taddi $" ++ baseReg ++ ", $zero, " ++ show stAddr ++ "\n"

-- data VarType = LVal | RVal  -- label of left or right value

data Expr
  = Val Int
  | Reg String
  | Var String Expr -- a variable or an array with index
  | Lt Expr Expr -- less than
  | Xor Expr Expr
  | Add Expr Expr


data Stmt
  = MACRO String
  | Define String Size -- define a variable, or an Array, with a data size
  | Assign Expr Expr
  | Inc Expr
  | IF Expr Stmt Stmt
  | For Expr Int Int Stmt -- For (Var _) st ed Block, the range is [st,ed)
  | Block [Stmt]
  | NOP

type Symbol = String
type Size = Int
type Addr = Int -- memory address
type LabelID = Int  -- for generating loop labels
type Code = String  -- the code generated

{- the environment of compiling progress -}
data Env = Env
  { symList :: [(Symbol, (Addr, Size))], -- symbol list
    heapTop :: Addr -- heap top address, for allocating
  }

{- the compiler state -}
type CompileSt = (Env, LabelID, Code) 

{- the compiler monad -}
{- the compiling environment should be passed by a state monad -}
type CompileM = State CompileSt

incLabel :: CompileM ()
incLabel = state $ \(e, l, c) -> ((), (e, l + 1, c))

-- add a new variable to an environment
newVariable :: Symbol -> Size -> CompileM ()
newVariable v size = do
  (Env s h, l, c) <- get
  case lookup v s of
    Nothing -> put (Env ((v, (h, size)) : s) (h + 4 * size), l, c)
    Just _ -> error $ "redefinition of symbol: " ++ v

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
env0 = Env [] 0

compileSt0 :: CompileSt
compileSt0 = (env0, 0, "")

(?=) :: Expr -> Expr -> Stmt
(?=) = Assign

(?<) :: Expr -> Expr -> Expr
(?<) = Lt

(?+) :: Expr -> Expr -> Expr
(?+) = Add

-- evaluate the expression in an environment
-- the result should be loaded to $t0
eval :: Env -> Expr -> String
eval _ (Val n) = show n
eval env (Var v idx) =
  case lookup v (symList env) of
    Nothing -> error $ "Undefined variable: " ++ v
    -- load variable to tmpreg 0
    -- eval variable (or array) with index
    Just (addr, size) -> 
      case idx of
        Val i ->
          if i < size then
            "\tlw $" ++ evalTmpReg ++ ", " ++ show (addr + 4*i) ++ "($" ++ baseReg ++ ")\n"
          else error $ "Array index out of range: " ++ v  -- this case is for arrays
        var@Var{} ->
          let evalIdx = eval env var  -- eval index to tmp
              mul4 = "\tsll $" ++ evalTmpReg ++ ", $" ++ evalTmpReg ++ ", 2\n"  -- TODO
              addToBase = eval env (Reg evalTmpReg ?+ Reg baseReg) in -- tmp = tmp + base
            evalIdx ++ mul4 ++ addToBase 
            ++ "\tlw $" ++ evalTmpReg ++ ", " ++ show addr ++ "($" ++ evalTmpReg ++ ")\n"
        _ -> error "TODO"

eval _ (Lt (Reg a) (Reg b)) =
  "\tslt $" ++ evalTmpReg ++ ", $" ++ a ++ ", $" ++ b ++ "\n"
eval _ (Lt (Reg a) (Val n)) =
  "\tslti $" ++ evalTmpReg ++ ", $" ++ a ++ ", " ++ show n ++ "\n"
eval env (Lt v@Var{} a@(Reg _)) =
  eval env v ++ eval env (Reg evalTmpReg ?< a) 
eval env (Lt v@Var{} n@(Val _)) =
  eval env v ++ eval env (Reg evalTmpReg ?< n)
eval env (Lt v1@Var{} v2@Var{}) =
  let movV2ToC1 = "\taddu $" ++ cmpReg1 ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
    in eval env v2 ++ movV2ToC1 ++ eval env (v1 ?< Reg cmpReg1)

{- xor-}
eval _ (Xor (Reg a) (Val n)) = 
  "\txori $" ++ evalTmpReg ++ ", $" ++ a ++ ", " ++ show n ++ "\n"

{- add -}
eval _ (Add (Reg a) (Val n)) =
  "\taddiu $" ++ evalTmpReg ++ ", $" ++ a ++ ", " ++ show n ++ "\n"
eval _ (Add (Reg a) (Reg b)) =
  "\taddu $" ++ evalTmpReg ++ ", $" ++ a ++ ", $" ++ b ++ "\n"
eval env (Add v@Var{} (Val n)) =
  eval env v ++ eval env (Reg evalTmpReg ?+ Val n)
eval env (Add v@Var{} r@(Reg _)) =
  eval env v ++ eval env (Reg evalTmpReg ?+ r)
eval env (Add v1@Var{} v2@Var{}) =
  let movV2ToC1 = "\taddu $" ++ cmpReg1 ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
    in eval env v2 ++ movV2ToC1 ++ eval env (v1 ?+ Reg cmpReg1)

{- errors -}
eval _ (Xor _ _) = error "Unknown expression pattern: xor"
eval _ _ = error "Unknown expression pattern"

-- yields the result and passing a new environment
compile :: Stmt -> CompileM ()
compile (MACRO s) = appendCode s
{- it changes the compiler state, with no result -}
compile (Define v s) = newVariable v s
{- assign register with imm -}
compile (Assign (Reg s) e@(Val _)) =
  do
    env <- getEnv
    appendCode $ "\taddi $" ++ s ++ ", $zero, " ++ eval env e ++ "\n"
{- assign register with register -}
compile (Assign (Reg r1) (Reg r2)) =
  appendCode $ "\taddu $" ++ r1 ++ ", $zero, $" ++ r2 ++ "\n"

{- assign register with variables -}
compile (Assign r@(Reg _) v@Var{}) =
  do
    env <- getEnv
    appendCode $ eval env v
    compile (r ?= Reg evalTmpReg)
    
{- assign variables with regs -}
compile (Assign (Var v idx) (Reg r)) =
  do
    env <- getEnv
    case lookup v (symList env) of
      Nothing -> error $ "Undefined variable: " ++ v
      {- store into memory -}
      Just (addr, size) -> 
        case idx of
          Val i ->
            if i < size then appendCode $ 
              "\tsw $" ++ r ++ ", " ++ show (addr + 4*i) ++ "($" ++ baseReg ++ ")\n"
            else error $ "Array index out of range: " ++ v  -- this case is for arrays
          var@Var{} -> do
            appendCode $ eval env var 
            appendCode $ "\tsll $" ++ evalTmpReg ++ ", $" ++ evalTmpReg ++ ", 2\n"  -- TODO
            compile (Reg addrTmpReg ?= Reg evalTmpReg)
            appendCode $ eval env (Reg addrTmpReg ?+ Reg baseReg)
            appendCode $ "\tsw $" ++ r ++ ", " ++ show addr ++ "($" ++ evalTmpReg ++ ")\n"
          _ -> error "TODO"

{- assign variables with imm -}
compile (Assign v@Var{} e@(Val _)) =
  do
    compile (Reg evalTmpReg ?= e)
    compile (v ?= Reg evalTmpReg)

{- otherwise: assign variables with expr -}
compile (Assign v@Var{} e) =
  do
    env <- getEnv
    appendCode $ eval env e
    -- use another temp reg since $t0 will be flushed while evaluating
    compile (Reg cmpReg1 ?= Reg evalTmpReg) 
    compile (v ?= Reg cmpReg1)

compile (Inc (Reg s)) =
  appendCode $ "\taddi $" ++ s ++ ", $" ++ s ++ ", 1" ++ "\n"
compile (Inc v@Var{}) =
  do
    env <- getEnv
    appendCode $ eval env v
    compile (Inc (Reg evalTmpReg))
    compile (v ?= Reg evalTmpReg)


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
    appendCode $ "\tj " ++ doneL ++ "\n\tnop\n"
    appendCode $ falseL ++ ":\n"  -- false label
    compile elseStmt
    appendCode $ doneL ++ ":\n" -- done label  


compile (For v@Var{} st ed bodyStmt) =
  if st > ed then error "Bad range in for statement"
  else do
    (env, curLabel, _) <- get 
    let label = "loop" ++ show curLabel
    incLabel  -- update label
    compile $ v ?= Val st
    appendCode $ label ++ ":\n"
    compile bodyStmt
    compile (Inc v)
    appendCode $ eval env (v ?< Val ed)
    appendCode $ eval env (Reg evalTmpReg `Xor` Val 1)  -- cond = not cond
    appendCode $ "\tblez $" ++ evalTmpReg ++ " " ++ label ++ "\n" -- if cond goto loop

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
