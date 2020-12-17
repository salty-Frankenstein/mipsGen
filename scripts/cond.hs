import MipsGen.Monadic

main :: IO ()
main = do
  writeFile "./cond.asm" $ runCompile $ 
    mDO $ do 
      mDEF "a"
      mDEF "b"
      var "a" ?= val 10
      mIF (var "a" ?< val 5) 
        (mDO $ do
          var "b" ?= val 1)
        (mDO $ do
          var "b" ?= val 2)