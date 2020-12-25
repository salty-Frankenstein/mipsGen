import MipsGen.Monadic

main :: IO ()
main = do
  writeFile ".\\scripts\\for_loop.asm" $ runCompile Main $ 
    mDO $ do 
      mMACRO "main:\n"
      mDEF "MAX"
      mDEF "cnt"
      var "MAX" ?= val 50
      var "cnt" ?= val 0
      mDEF "i"
      mFOR(var "i" ^= val 0, var "i" ?< val 50, inc $ var "i") $ mDO $ do
        mDEF "j"
        mFOR(var "j" ^= val 0, var "j" ?< val 50, inc $ var "j") $ mDO $ do
          mIF (var "j" ?< var "i")
            (inc $ var "cnt")
            (mDO $ mINC (var "cnt") >> mINC (var "cnt"))

      mDEF "b"
      var "b" ?= val 0
      mFORR (var "i") 0 50 $ mDO $ do
        mDEF "j"
        mFORR (var "j") 0 50 $ mDO $ do
          mIF (var "j" ?< var "i") 
            (inc $ var "b") 
            (mDO $ mINC (var "b") >> mINC (var "b"))
      