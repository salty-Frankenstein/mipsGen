{- for monadic syntax -}
module MipsGen.Monadic (
    runCompile,
    mDEF, mARR, mWHILE, mIF, mFOR, mFORR, mDO, (?!), (?=), mINC, mNOP, 
    (^=),  --non monadic version
    _if, _while, --non monadic version
    (?<), (?<=), (?==), (?!=), (?&&), (?+),
    var, arr, val, chr, reg, nop, inc,
    StmtM
) where

import Control.Monad.Writer
import MipsGen.MipsGen hiding ((?=))
import Data.Char (ord)

type StmtM = Writer [Stmt] ()

var :: String -> Expr
var s = Var s (Val 0)

chr :: Char -> Expr
chr c = Val $ ord c

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

infixr 0 ^=
(^=) :: Expr -> Expr -> Stmt
(^=) = Assign


mINC :: Expr -> StmtM
mINC e = tell [Inc e]

mIF :: Expr -> Stmt -> Stmt -> StmtM
mIF c s1 s2 = tell [IF c s1 s2]

mWHILE :: Expr -> Stmt -> StmtM
mWHILE c b = tell [While c b]

mFOR :: (Stmt, Expr, Stmt) -> Stmt -> StmtM
mFOR (s, c, u) b = tell [For s c u b]

mFORR :: Expr -> Int -> Int -> Stmt -> StmtM
mFORR c st ed s = tell [ForR c st ed s]

mNOP :: StmtM
mNOP = tell [NOP]

mDO :: StmtM -> Stmt
mDO stmt = Block $ snd $ runWriter stmt

_if = IF
_while = While