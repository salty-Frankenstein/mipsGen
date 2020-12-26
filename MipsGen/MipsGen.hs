module MipsGen.MipsGen where

import Control.Monad.State

debug = False

stAddr = if debug then 0x10040000 else 0x30000

stackReg = "sp"
frameReg = "fp"
retValReg = "v0"
retAddrReg = "ra"

{- TODO: change into saved regs -}
evalTmpReg = "t0"
tmpReg2 = "t5"
leftReg = "t6"
rightReg = "t7"
addrTmpReg = "t3"

cmpReg1 = "t1"
cmpReg2 = "t2"
cmpReg3 = "t4"

data Mode = Main | Lib

{- initialize stack -}
initCode :: Code
initCode = "\taddi $" ++ frameReg ++ ", $zero, " ++ show (stAddr-4) ++ "\n"
  ++ "\taddi $" ++ stackReg ++ ", $zero, " ++ show (stAddr-4) ++ "\n"
  ++ "\tj main\n" 
      
data Expr
  = Val Int
  | Reg String
  | Var Symbol Expr -- a variable or an array with index
  | Call Symbol [Expr]    -- call a procedure, with args epressions
  | CallExt Symbol [Expr] -- call a external procedure, without compile-time checking
  | Deref Expr  -- dereference of pointer
  | Ref Expr  -- get the address of a variable
  | Lt Expr Expr    -- less than
  | Le Expr Expr    -- less or equal
  | Equal Expr Expr
  | NEqual Expr Expr  -- not equal
  | And Expr Expr -- logical and
  | Xor Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr


data Stmt
  = MACRO String
  | Define Symbol Size -- define a variable, or an Array, with a data size
  | Proc Symbol [Symbol] Stmt -- define a procedure
  | Return Expr   -- return the result of expr
  | Assign Expr Expr
  | Write Expr Expr   -- write a value into an address
  | Inc Expr
  | IF Expr Stmt Stmt
  | While Expr Stmt   -- while loop
  | ForR Expr Int Int Stmt -- ForR (Var _) st ed Block, the range is [st,ed)
  | For Stmt Expr Stmt Stmt  -- real C for-loop: For begin cond update body
  | Block [Stmt]
  | NOP

type Symbol = String
type Size = Int
type Addr = Int -- memory address
type LabelID = Int  -- for generating loop labels
type Code = String  -- the code generated

{- the environment of compiling progress -}
data Env = Env {
  symList :: [(Symbol, (Addr, Size))],  -- symbol list for variables and arrays
  funcList :: [(Symbol, Int)] -- function name list, for function arg number
} 

{- the compiler state -}
data CompileSt = ComST Env LabelID Addr Code

{- the compiler monad -}
{- the compiling environment should be passed by a state monad -}
type CompileM = State CompileSt

incLabel :: CompileM ()
incLabel = state $ \(ComST e l h c) -> ((), ComST e (l + 1) h c)

{-
    stack frame for proc call:
       old fp ->     -------------
     (fp+arg_offset) |arg n to 1 |
     (fp-4)          |ret        |
           fp ->     |old fp     |
     (fp+var_offset) |auto vars  |
           sp ->     |...        |
    
    look up address for a symbol: fp+offset
-}

-- define the procedure parameter, 
-- adding a list symbol name and the negative address offset of the parameter into the environment
-- in a reversed order
-- and do nothing for the offset of auto variables
defineParameter :: [Symbol] -> CompileM ()
defineParameter p = defineParameter' p (-8) -- the last parameter start at fp-8
  where 
    defineParameter' [] _ = return ()
    defineParameter' (x:xs) offset = 
      do
        (Env s f) <- getEnv
        case lookup x s of
          Nothing -> putEnv (Env ((x, (offset, 1)) : s) f)
          Just _ -> error $ "redefinition of parameter symbol: " ++ x
        defineParameter' xs (offset-4)

-- add a new variable to an environment
newVariable :: Symbol -> Size -> CompileM ()
newVariable v size = do
  (ComST (Env s f) l h c) <- get
  case lookup v s of
    Nothing -> do
      put $ ComST (Env ((v, (h, size)) : s) f) l (h + 4 * size) c
      appendCode $ "\taddi $" ++ stackReg ++ ", $" ++ stackReg ++ ", " ++ show (4*size) ++ "\n"
    Just _ -> error $ "redefinition of symbol: " ++ v

-- define a proc with name and number of args
newProcedure :: Symbol -> Int -> CompileM ()
newProcedure p n = do
  (ComST (Env s f) l h c) <- get
  case lookup p f of
    Nothing -> put $ ComST (Env s $ (p, n) : f) l h c
    Just _ -> error $ "redefinition of procedure: " ++ p

appendCode :: String -> CompileM ()
appendCode c' = state $ \(ComST e l h c) -> ((), ComST e l h (c ++ c'))

getEnv :: CompileM Env
getEnv = do
  ComST e _ _ _ <- get
  return e

putEnv :: Env -> CompileM ()
putEnv e' = state $ \(ComST _ l h c) -> ((), ComST e' l h c)

-- empty environment

env0 :: Env
env0 = Env [] []

compileSt0 :: CompileSt
compileSt0 = ComST env0 0 4 ""

(?=) :: Expr -> Expr -> Stmt
(?=) = Assign

(?<-) :: Expr -> Expr -> Stmt
(?<-) = Write

infixr 4 ?<
(?<) :: Expr -> Expr -> Expr
(?<) = Lt

infixr 4 ?<=
(?<=) :: Expr -> Expr -> Expr
(?<=) = Le

infixr 4 ?==
(?==) :: Expr -> Expr -> Expr
(?==) = Equal

infix 4 ?!=
(?!=) :: Expr -> Expr -> Expr
(?!=) = NEqual

infixr 3 ?&&
(?&&) :: Expr -> Expr -> Expr
(?&&) = And

infixl 6 ?+
infixl 6 ?-
infixl 7 ?*
infixl 7 ?/
(?+), (?-), (?*), (?/) :: Expr -> Expr -> Expr
(?+) = Add
(?-) = Sub
(?*) = Mul
(?/) = Div

{- stack operations -}
pushReg :: String -> String
pushReg reg = "\taddi $" ++ stackReg ++ ", $" ++ stackReg ++ ", 4\n"
  ++"\tsw $" ++ reg ++ ", ($" ++ stackReg ++ ")\n"

popReg :: String -> String
popReg reg = "\tlw $" ++ reg ++ ", ($" ++ stackReg ++ ")\n"
  ++ "\taddi $" ++ stackReg ++ ", $" ++ stackReg ++ ", -4\n"


-- evaluate the expression in an environment
-- the result should be loaded to $t0
eval :: Env -> Expr -> String
eval _ (Val n) = 
  "\taddi $" ++ evalTmpReg ++ ", $zero, " ++ show n ++ "\n"
eval env (Var v idx) =
  case lookup v (symList env) of
    Nothing -> error $ "Undefined variable: " ++ v
    -- load variable to tmpreg 0
    -- eval variable (or array) with index
    Just (addr, size) -> 
      case idx of
        Val i ->
          if i < size then
            "\tlw $" ++ evalTmpReg ++ ", " ++ show (addr + 4*i) ++ "($" ++ frameReg ++ ")\n"
          else error $ "Array index out of range: " ++ v  -- this case is for arrays
        var ->  -- it can be any expr
          let evalIdx = eval env var  -- eval index to tmp
              mul4 = "\tsll $" ++ evalTmpReg ++ ", $" ++ evalTmpReg ++ ", 2\n"  -- TODO
              addToBase = eval env (Reg evalTmpReg ?+ Reg frameReg) in -- tmp = tmp + base
            evalIdx ++ mul4 ++ addToBase 
            ++ "\tlw $" ++ evalTmpReg ++ ", " ++ show addr ++ "($" ++ evalTmpReg ++ ")\n"
        -- _ -> error "TODO"

{- dereference -}
eval env (Deref e) = 
  eval env e
  ++ "\tlw $" ++ evalTmpReg ++ ", ($" ++ evalTmpReg ++ ")\n"  

eval env (Ref (Var v _)) =
  case lookup v (symList env) of
    Nothing -> error $ "Undefined variable: " ++ v
    Just (addr, size) ->
      "\taddi $" ++ evalTmpReg ++ ", $" ++ frameReg ++ ", " ++ show addr ++ "\n"

{- less than -}
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
{- otherwise -}
eval env (Lt e1 e2)= 
  eval env e1 
  ++ "\taddu $" ++ leftReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env e2
  ++ "\taddu $" ++ rightReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env (Reg leftReg ?< Reg rightReg)

{- less || equal -}
{-
  a <= b
  not (b < a)
-}
eval env (Le e1 e2) =
  eval env (e2 ?< e1)
  ++ "\txori $" ++ evalTmpReg ++ ", $" ++ evalTmpReg ++ ", 1\n" -- t0 <- not t0

{- equal -}
{- 
        a == b
    1.  cmp1 <- a < b
    2.  cmp2 <- b < a
    3.  cmp1 == 0 && cmp2 == 0 <=> a == b
        <=> not cmp1 && not cmp2
        <=> not (cmp1 || cmp2)
        <=> nor cmp1 cmp2 <=> a == b
    since nor is bitwise, only the low 1 bit should be kept, thus
    4.  res <- (res << 31) >> 31
-}
eval env (Equal (Reg a) (Reg b)) =
  let step1 = "\tslt $" ++ cmpReg1 ++ ", $" ++ a ++ ", $" ++ b ++ "\n"
      step2 = "\tslt $" ++ cmpReg2 ++ ", $" ++ b ++ ", $" ++ a ++ "\n"
      step3 = "\tnor $" ++ evalTmpReg ++ ", $" ++ cmpReg1 ++ ", $" ++ cmpReg2 ++ "\n"
      step4 = "\tsll $" ++ evalTmpReg ++ ", $" ++ evalTmpReg ++ ", 31\n"
            ++"\tsrl $" ++ evalTmpReg ++ ", $" ++ evalTmpReg ++ ", 31\n"
    in step1 ++ step2 ++ step3 ++ step4
eval env (Equal r@(Reg _) (Val n)) =
  "\taddi $" ++ cmpReg3 ++ ", $zero, " ++ show n ++ "\n"
  ++ eval env (r ?== Reg cmpReg3)
eval env (Equal v@Var{} a@(Reg _)) =
  eval env v ++ eval env (Reg evalTmpReg ?== a) 
eval env (Equal v@Var{} n@(Val _)) =
  eval env v ++ eval env (Reg evalTmpReg ?== n)
eval env (Equal v1@Var{} v2@Var{}) =
  let movV2ToC3 = "\taddu $" ++ cmpReg3 ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
    in eval env v2 ++ movV2ToC3 ++ eval env (v1 ?== Reg cmpReg3)
{- otherwise -}
eval env (Equal e1 e2)= 
  eval env e1 
  ++ "\taddu $" ++ leftReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env e2
  ++ "\taddu $" ++ rightReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env (Reg leftReg ?== Reg rightReg)

{- not equal -}
eval env (NEqual e1 e2) =
  eval env (Equal e1 e2)  -- t0 <- e1 == e2
  ++ "\txori $" ++ evalTmpReg ++ ", $" ++ evalTmpReg ++ ", 1\n" -- t0 <- not t0

{- and -}
{-
    nor a b = !(a || b) = !a && !b
    and a b = !(!a) && !(!b) = !(!a || !b) = nor (!a) (!b)

    1. t1 <- xor a 1
    2. t2 <- xor b 1
    3. t0 <- nor t1 t2
    4. t0 <- (t0 << 31) >> 31 
-}
eval env (And (Reg a) (Reg b)) = 
  let step1 = "\txori $" ++ cmpReg1 ++ ", $" ++ a ++ ", 1\n"
      step2 = "\txori $" ++ cmpReg2 ++ ", $" ++ b ++ ", 1\n"
      step3 = "\tnor $" ++ evalTmpReg ++ ", $" ++ cmpReg1 ++ ", $" ++ cmpReg2 ++ "\n"
      step4 = "\tsll $" ++ evalTmpReg ++ ", $" ++ evalTmpReg ++ ", 31\n"
            ++"\tsrl $" ++ evalTmpReg ++ ", $" ++ evalTmpReg ++ ", 31\n"
      in step1 ++ step2 ++ step3 ++ step4
eval env (And r@(Reg _) (Val n)) =
  "\taddi $" ++ cmpReg3 ++ ", $zero, " ++ show n ++ "\n"
  ++ eval env (r ?&& Reg cmpReg3)
eval env (And v@Var{} a@(Reg _)) =
  eval env v ++ eval env (Reg evalTmpReg ?&& a) 
eval env (And v@Var{} n@(Val _)) =
  eval env v ++ eval env (Reg evalTmpReg ?&& n)
eval env (And v1@Var{} v2@Var{}) =
  let movV2ToC3 = "\taddu $" ++ cmpReg3 ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
    in eval env v2 ++ movV2ToC3 ++ eval env (v1 ?&& Reg cmpReg3)
{- otherwise -}
eval env (And e1 e2)= 
  eval env e1 
  ++ "\taddu $" ++ leftReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env e2
  ++ "\taddu $" ++ rightReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env (Reg leftReg ?&& Reg rightReg)

{- xor -}
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
{- otherwise -}
eval env (Add e1 e2)= 
  eval env e1 
  ++ "\taddu $" ++ leftReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env e2
  ++ "\taddu $" ++ rightReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env (Reg leftReg ?+ Reg rightReg)

{- sub -}
eval env (Sub (Reg a) (Val n)) = 
  eval env (Reg a ?+ Val (-n))
eval _ (Sub (Reg a) (Reg b)) = 
  "\tsubu $" ++ evalTmpReg ++ ", $" ++ a ++ ", $" ++ b ++ "\n"
eval env (Sub v@Var{} (Val n)) =
  eval env (v ?+ Val (-n))
{- otherwise -}
eval env (Sub e1 e2)= 
  eval env e1 
  ++ "\taddu $" ++ leftReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env e2
  ++ "\taddu $" ++ rightReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env (Reg leftReg ?- Reg rightReg)

{- mul -}
eval _ (Mul (Reg a) (Reg b)) = 
  "\tmult $" ++ a ++ ", $" ++ b ++ "\n"
  ++ "\tmflo $" ++ evalTmpReg ++ "\n"

{- otherwise -}
eval env (Mul e1 e2)= 
  eval env e1 
  ++ "\taddu $" ++ leftReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env e2
  ++ "\taddu $" ++ rightReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env (Reg leftReg ?* Reg rightReg)

{- div -}
eval _ (Div (Reg a) (Reg b)) =
  "\tdivu $" ++ evalTmpReg ++ ", $" ++ a ++ ", $" ++ b ++ "\n"
{- otherwise -}
eval env (Div e1 e2)= 
  eval env e1 
  ++ "\taddu $" ++ leftReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env e2
  ++ "\taddu $" ++ rightReg ++ ", $zero, $" ++ evalTmpReg ++ "\n" 
  ++ eval env (Reg leftReg ?/ Reg rightReg)

{- call expr -}
eval env@(Env s f) (Call name args) = 
  case lookup name f of
    Nothing -> error $ "Undefined procedure: " ++ name
    Just n -> 
      if n /= length args then error $ "Can match arg number: " ++ name
      else pushArgs (reverse args)
      ++ "\tjal " ++ name ++ "\n"  -- jump to function
      ++ "\taddu $" ++ evalTmpReg ++ ", $zero, $" ++ retValReg ++ "\n"  -- assign return value
  where
    pushArgs :: [Expr] -> String
    pushArgs [] = ""
    pushArgs (x:xs) = 
      eval env x
      ++ pushReg evalTmpReg
      ++ pushArgs xs

{- call expr -}
eval env@(Env s f) (CallExt name args) = 
  pushArgs (reverse args)
  ++ "\tjal " ++ name ++ "\n"  -- jump to function
  ++ "\taddu $" ++ evalTmpReg ++ ", $zero, $" ++ retValReg ++ "\n"  -- assign return value
  where
    pushArgs :: [Expr] -> String
    pushArgs [] = ""
    pushArgs (x:xs) = 
      eval env x
      ++ pushReg evalTmpReg
      ++ pushArgs xs

{- errors -}
eval _ (Xor _ _) = error "Unknown expression pattern: xor"
eval _ _ = error "Unknown expression pattern"

-- yields the result and passing a new environment
compile :: Stmt -> CompileM ()
compile (MACRO s) = appendCode s
{- it changes the compiler state, with no result -}
compile (Define v s) = newVariable v s
{- assign register with imm -}
compile (Assign (Reg s) (Val n)) =
  do
    env <- getEnv
    appendCode $ "\taddi $" ++ s ++ ", $zero, " ++ show n ++ "\n"
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
              "\tsw $" ++ r ++ ", " ++ show (addr + 4*i) ++ "($" ++ frameReg ++ ")\n"
            else error $ "Array index out of range: " ++ v  -- this case is for arrays
          var@Var{} -> do
            appendCode $ eval env var 
            appendCode $ "\tsll $" ++ evalTmpReg ++ ", $" ++ evalTmpReg ++ ", 2\n"  -- TODO
            compile (Reg addrTmpReg ?= Reg evalTmpReg)
            appendCode $ eval env (Reg addrTmpReg ?+ Reg frameReg)
            appendCode $ "\tsw $" ++ r ++ ", " ++ show addr ++ "($" ++ evalTmpReg ++ ")\n"
          _ -> error "TODO"

{- assign variables with imm -}
compile (Assign v@Var{} e@(Val _)) =
  do
    -- use another temp reg since $t0 will be flushed while evaluating
    compile (Reg tmpReg2 ?= e)
    compile (v ?= Reg tmpReg2)

{- otherwise: assign variables with expr -}
compile (Assign v@Var{} e) =
  do
    env <- getEnv
    appendCode $ eval env e
    -- use another temp reg since $t0 will be flushed while evaluating
    compile (Reg tmpReg2 ?= Reg evalTmpReg) 
    compile (v ?= Reg tmpReg2)

{- write into address -}
compile (Write e1 e2) =
  do
    env <- getEnv
    appendCode $ eval env e1
    compile (Reg tmpReg2 ?= Reg evalTmpReg) 
    appendCode $ eval env e2
    appendCode $ "\tsw $" ++ evalTmpReg ++ ", ($" ++ tmpReg2 ++ ")\n"

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
    ComST env curLabel _ _ <- get
    appendCode $ eval env cond -- condition expr, result in $t0
    let lID = show curLabel
        falseL = "false_label" ++ lID
        doneL = "done_label" ++ lID
    appendCode $ "\tblez $" ++ evalTmpReg ++ " " ++ falseL ++ "\n" -- if !cond goto false_label
    compile NOP --
    incLabel  -- update label
    compile thenStmt
    appendCode $ "\tj " ++ doneL ++ "\n\tnop\n"
    appendCode $ falseL ++ ":\n"  -- false label
    compile elseStmt
    appendCode $ doneL ++ ":\n" -- done label  

{- while loop -}
compile (While cond body) =
  do
    ComST env curLabel _ _ <- get 
    let label = show curLabel
        loopL = "loop" ++ label
        doneL = "done" ++ label
    incLabel  -- update label
    appendCode $ eval env cond  -- judge condition
    appendCode $ "\tblez $" ++ evalTmpReg ++ " " ++ doneL ++ "\n" -- if !cond goto done
    compile NOP --
    appendCode $ loopL ++ ":\n"
    compile body
    appendCode $ eval env cond  -- judge condition
    appendCode $ eval env (Reg evalTmpReg `Xor` Val 1)  -- cond = not cond
    appendCode $ "\tblez $" ++ evalTmpReg ++ " " ++ loopL ++ "\n" -- if cond goto loop
    compile NOP --
    appendCode $ doneL ++ ":\n"

{- for range loop -}
compile (ForR v@Var{} st ed bodyStmt) =
  if st > ed then error "Bad range in for-range statement"
  else do
    ComST env curLabel _ _ <- get 
    let label = "loop" ++ show curLabel
    incLabel  -- update label
    compile $ v ?= Val st
    appendCode $ label ++ ":\n"
    compile bodyStmt
    compile (Inc v)
    appendCode $ eval env (v ?< Val ed)
    appendCode $ eval env (Reg evalTmpReg `Xor` Val 1)  -- cond = not cond
    appendCode $ "\tblez $" ++ evalTmpReg ++ " " ++ label ++ "\n" -- if cond goto loop
    compile NOP --

{- for loop -}
compile (For begin cond update body) =
  do
    ComST env curLabel _ _ <- get 
    let label = show curLabel
        loopL = "loop" ++ label
        doneL = "done" ++ label
    incLabel  -- update label 
    compile begin   -- begin statement
    appendCode $ eval env cond  -- judge condition
    appendCode $ "\tblez $" ++ evalTmpReg ++ " " ++ doneL ++ "\n" -- if !cond goto done
    compile NOP --
    appendCode $ loopL ++ ":\n"
    compile body
    compile update
    appendCode $ eval env cond
    appendCode $ eval env (Reg evalTmpReg `Xor` Val 1)  -- cond = not cond
    appendCode $ "\tblez $" ++ evalTmpReg ++ " " ++ loopL ++ "\n" -- if cond goto loop
    compile NOP --
    appendCode $ doneL ++ ":\n"

{- define a procedure -}
compile (Proc name para body) = 
  do
    newProcedure name (length para)   -- define the procedure
    ComST (Env olds oldf) _ olda _ <- get
    appendCode $ name ++ ":\n"

    appendCode $ pushReg retAddrReg   -- push return address
    appendCode $ pushReg frameReg     -- push old fp
    compile (Reg frameReg ?= Reg stackReg)  -- create a new frame

    putEnv (Env [] oldf)     -- set a new frame, with new variable table and offset 
    defineParameter para     -- define the parameters' address

    compile body -- the return prosess shall be included the body stmt
    -- recover the original env
    state $ \(ComST (Env _ newf) l a c) -> ((), ComST (Env olds newf) l olda c)  

compile (Return e) =
  do
    env <- getEnv
    appendCode $ eval env e   -- eval the expression
    compile (Reg retValReg ?= Reg evalTmpReg) -- assign to the return value
    compile (Reg stackReg ?= Reg frameReg)  -- recover the frame
    appendCode $ popReg frameReg    -- pop out the old fp
    appendCode $ popReg retAddrReg  -- pop out the return address
    appendCode "\tjr $ra\n" -- return

compile (Block []) = return () -- do nothing
compile (Block (x : xs)) =
  do
    ComST oldEnv _ _ _ <- get -- saving the old environment
    compile x
    compile (Block xs) -- complie the whole block with the environment changed
    putEnv oldEnv -- set the original env for scoping(??)

compile NOP = appendCode "\tnop\n"
{- errors -}
compile (Assign _ _) = error "cannot assign to a right value"
compile (Inc _) = error "cannot assign to a right value"
compile ForR {} = error "syntax error in for-range-statement"

runCompile :: Mode -> Stmt -> String
runCompile m s =
  let ComST _ _ _ c = execState (compile s) compileSt0
   in case m of
     Main -> initCode ++ c
     _ -> c
