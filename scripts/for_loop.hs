import MipsGen.Monadic

main :: IO ()
main = do
  writeFile "./for_loop.asm" $ runCompile $ 
    mDO $ do 
      mDEF "a"
      mDEF "i"
      mFOR (var "i") 1 10 $ mDO $ do
        mINC $ var "a"
    