import MipsGen.Monadic

main :: IO ()
main = do
  writeFile "./fib.asm" $ runCompile $ 
    mDO $ do
      mARR "a" 20
      arr "a" ?! val 0 ?= val 0   -- a[0] = 0
      arr "a" ?! val 1 ?= val 1   -- a[1] = 1
      -- arr "a" ?! val 2 ?= arr "a" ?
      -- mDEF "i"          -- define loop variable
      -- mFOR (var "i") 2 40 $ mDO $ do  -- for i in range(0, 40)
      --   arr "a" ?! var "i" ?= arr "a" ?! var "i" ?+ val (-1) 

      -- mARR "fibs" 100   -- define an array
      -- mDEF "i"          -- define loop variable
      -- mFOR (var "i") 0 40 $ mDO $ do  -- for i in range(0, 40)
      --   arr "a" ?! var "i" 
      --   mNOP