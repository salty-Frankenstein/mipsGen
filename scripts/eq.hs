import MipsGen.Monadic

main :: IO ()
main = do
  writeFile ".\\scripts\\eq.asm" $ runCompile Main $ 
    mDO $ do 
      mMACRO "main:\n"
      mDEF "a"
      mDEF "b"
      var "a" ?= val 1
      var "b" ?= val 2
      mDEF "eq" 
      var "eq" ?= var "a" ?== var "b"
      var "b" ?= val (-1)
      var "eq" ?= var "a" ?== var "b"
      var "b" ?= val 1
      var "eq" ?= var "a" ?== var "b"
      