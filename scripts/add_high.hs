import MipsGen.Monadic

main :: IO ()
main = do
  writeFile "./add_high.asm" $ runCompile $ 
    mDO $ do
      let max = 100   -- constant
      mARR "a" max
      mDEF "alen"

      mARR "b" max
      mDEF "blen"
      
      mARR "ans" max
      mDEF "anslen"

      mDEF "i"
      {- init -}
      mFOR(var "i" ^= val 0, var "i" ?< val max, inc $ var "i") $ mDO $ do
        arr "a" ?! var "i" ?= val 0
        arr "b" ?! var "i" ?= val 0
        arr "ans" ?! var "i" ?= val 0

      -- a == 17
      arr "a" ?! val 0 ?= val 7
      arr "a" ?! val 1 ?= val 1
      var "alen" ?= val 2
      -- b == 8
      arr "b" ?! val 0 ?= val 8
      var "blen" ?= val 1

      mDEF "carry"
      var "carry" ?= val 0

      mDEF "maxlen"
      mIF(var "alen" ?< var "blen")
        (var "maxlen" ^= var "blen")
        (var "maxlen" ^= var "alen")
      
      mFOR(var "i" ^= val 0, 
          var "i" ?< var "maxlen",
          inc $ var "i") $ mDO $ do
        let ai = arr "a" ?! var "i"
            bi = arr "b" ?! var "i"
            ansi = arr "ans" ?! var "i"
            sumAB = ai ?+ bi
        mDEF "t"
        var "t" ?= sumAB
        ansi ?= var "t" ?+ var "carry"
        mIF(ansi ?< val 10)
          (var "carry" ^= val 0)
          (mDO $ do
            var "carry" ?= val 1
            ansi ?= ansi ?+ val (-10))
        
      mIF(var "carry")
        (mDO $ do
          arr "ans" ?! var "i" ?= var "carry"
          mINC $ var "i")
        nop

      var "anslen" ?= var "i"
