import MipsGen.Monadic
import Main.Fact
import Main.String
import Main.IO
import Data.Char ( ord ) 

includes :: IO String
includes = do
  f1 <- readFile ".\\Main\\fact.asm"
  return f1 
  return ""

definitions :: StmtM
definitions = do
  fact
  stringLen
  stringCmp
  numToStr  
  includeIO

mainLoop :: Stmt
mainLoop = 
  mDO $ do
    definitions

    mMACRO "main:\n"
    mARR "str" 20
    mDEF "res"

    var "res" ?= call "numToStr" [ref $ var "str", val 0]
    -- mDEF "res"
    
    -- mARR "str1" 20
    -- mARR "str2" 20
    -- stringInit "str1" "abcde"
    -- stringInit "str2" "abdde"

    -- var "res" ?= call "stringCmp" [ref $ var "str1", ref $ var "str2"]

    mWHILE(val 1) $ mDO $ do  -- main loop

      mNOP

main :: IO ()
main = do
  let mainCode = runCompile Main mainLoop
  includeCode <- includes
  writeFile ".\\Main\\main_loop.asm" $ mainCode ++ includeCode
    