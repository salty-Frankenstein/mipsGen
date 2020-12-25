import MipsGen.Monadic
import Main.Fact

includes :: IO String
includes = do
  f1 <- readFile ".\\Main\\fact.asm"
  return f1 
  return ""

definitions :: StmtM
definitions = do
  fact

mainLoop :: Stmt
mainLoop = 
  mDO $ do
    definitions

    mMACRO "main:\n"

    mDEF "res"
    var "res" ?= call "fact" [val 10]

    mWHILE(val 1) $ mDO $ do  -- main loop
      mNOP

main :: IO ()
main = do
  let mainCode = runCompile Main $ mainLoop
  includeCode <- includes
  writeFile ".\\Main\\main_loop.asm" $ mainCode ++ includeCode
    