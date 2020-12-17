{- for monadic syntax -}
module MipsGen.Monadic (
    runCompile,
    mDEF, mIF, mFOR, mDO, (?=), mINC, mNOP, (?<),
    var, val, reg, nop, inc
) where

import Control.Monad.Writer
import MipsGen.MipsGen hiding ((?=))

type StmtM = Writer [Stmt] ()

var = Var
val = Val
reg = Reg
nop = NOP
inc = Inc

mDEF :: String -> StmtM
mDEF n = tell [Define $ Var n]

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