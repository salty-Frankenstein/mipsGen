{- for monadic syntax -}
module MipsGen.Monadic (
    runCompile,
    mDEF, mIF, mDO, (?=), mNOP, (?<=),
    var, val, reg, nop
) where

import Control.Monad.Writer
import MipsGen.MipsGen hiding ((?=))

type StmtM = Writer [Stmt] ()

var = Var
val = Val
reg = Reg
nop = NOP

mDEF :: String -> StmtM
mDEF n = tell [Define $ Var n]

(?=) :: Expr -> Expr -> StmtM
(?=) e1 e2 = tell [Assign e1 e2]

mIF :: Expr -> Stmt -> Stmt -> StmtM
mIF c s1 s2 = tell [IF c s1 s2]

mNOP :: StmtM
mNOP = tell [NOP]

mDO :: StmtM -> Stmt
mDO stmt = Block $ snd $ runWriter stmt