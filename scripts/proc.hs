import MipsGen.Monadic

main :: IO ()
main = do
  writeFile ".\\scripts\\proc.asm" $ runCompile $ 
    mDO $ do
      mPROC "fact" ["n"] $ mDO $ do
        mIF(var "n" ?== val 0)
          (_return $ val 1) 
          (mDO $ do
            mDEF "t"
            var "t" ?= call "fact" [var "n" ?- val 1]
            mRET $ var "n" ?* var "t")

      mPROC "add" ["a", "b"] $ mDO $ do
        mRET $ var "a"

      mMACRO "main:\n"
      mDEF "res"
      var "res" ?= call "fact" [val 10]

      mDEF "x"
      var "x" ?= val 1
      mDEF "y"
      var "y" ?= val 2
      var "res" ?= call "add" [var "x", var "y"]