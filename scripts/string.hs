import MipsGen.Monadic
import Data.Char (ord)


charConst :: StmtM 
charConst = do
  mDEF "chr0" >> mDEF "chr9"
  var "chr0" ?= chr '0'
  var "chr9" ?= chr '9'

main :: IO ()
main = do
  writeFile "./eval_expr.asm" $ runCompile $ 
    mDO $ do
      return ()