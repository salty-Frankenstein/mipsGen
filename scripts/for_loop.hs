import MipsGen.Monadic

main :: IO ()
main = do
  writeFile "./for_loop.asm" $ runCompile $ 
    mDO $ do 
      mDEF "b"
      mDEF "i"
      var "b" ?= val 0
      mFOR (var "i") 0 50 $ mDO $ do
        mDEF "j"
        mFOR (var "j") 0 50 $ mDO $ do
          mIF (var "j" ?< var "i") 
            (inc $ var "b") 
            (mDO $ mINC (var "b") >> mINC (var "b"))
      