import MipsGen.Monadic

main :: IO ()
main = do
  writeFile "./eq.asm" $ runCompile $ 
    mDO $ do 
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
      