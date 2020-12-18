import MipsGen.Monadic

main :: IO ()
main = do
  writeFile "./fib.asm" $ runCompile $ 
    mDO $ do
      mARR "fibs" 20                            -- define an array
      arr "fibs" ?! val 0 ?= val 0              -- fibs[0] = 0
      arr "fibs" ?! val 1 ?= val 1              -- fibs[1] = 1
      mDEF "i"                                  -- define loop variable
      mFOR (var "i") 2 20 $ mDO $ do            -- for i in range(0, 40)
        mDEF "i-1"
        mDEF "i-2"
        var "i-1" ?= var "i" ?+ val (-1)
        var "i-2" ?= var "i" ?+ val (-2)
        let fib_i_1 = arr "fibs" ?! var "i-1"   -- binding expressions :)
            fib_i_2 = arr "fibs" ?! var "i-2"
        arr "fibs" ?! var "i" ?= fib_i_1 ?+ fib_i_2 
