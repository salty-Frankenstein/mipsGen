import MipsGen.Monadic

main :: IO ()
main = do
  writeFile "./while_loop.asm" $ runCompile $
    mDO $ do
      mDEF "MAX"
      mDEF "cnt"
      var "MAX" ?= val 50
      var "cnt" ?= val 0
      mDEF "i"
      var "i" ?= val 0
      mWHILE(var "i" ?< var "MAX") $ mDO $ do
        mDEF "j"
        var "j" ?= val 0
        mWHILE(var "j" ?< var "MAX") $ mDO $ do
          mIF(var "j" ?< var "i")
            (inc $ var "cnt")
            (mDO $ do
                mINC $ var "cnt"
                mINC $ var "cnt")
          mINC $ var "j"
        mINC $ var "i"