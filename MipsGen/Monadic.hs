{- for monadic syntax -}
module MipsGen.Monadic (
    runCompile,
    mDEF, mARR, mIF, mFOR, mDO, (?!), (?=), mINC, mNOP, 
    (?<), (?==), (?+),
    var, arr, val, reg, nop, inc
) where

import Control.Monad.Writer
import MipsGen.MipsGen hiding ((?=))

type StmtM = Writer [Stmt] ()

var :: String -> Expr
var s = Var s (Val 0)
val = Val
reg = Reg
nop = NOP
inc = Inc

newtype Arr = Arr String
arr = Arr

-- define a variable
mDEF :: String -> StmtM
mDEF n = tell [Define n 1]

-- define an array
mARR :: String -> Size -> StmtM
mARR n s = tell [Define n s]

infixl 9 ?!
(?!) :: Arr -> Expr -> Expr 
(?!) (Arr a) = Var a

infixr 0 ?=
(?=) :: Expr -> Expr -> StmtM
(?=) e1 e2 = tell [Assign e1 e2]

mINC :: Expr -> StmtM
mINC e = tell [Inc e]

mIF :: Expr -> Stmt -> Stmt -> StmtM
mIF c s1 s2 = tell [IF c s1 s2]

mFOR :: Expr -> Int -> Int -> Stmt -> StmtM
mFOR c st ed s = tell [For c st ed s]

mNOP :: StmtM
mNOP = tell [NOP]

mDO :: StmtM -> Stmt
mDO stmt = Block $ snd $ runWriter stmt