module Main.Fib where
import MipsGen.Monadic

fib :: StmtM
fib = do
  mPROC "fib" ["n", "resptr"] $ mDO $ do
    let res = var "resptr"

    let max = 100   -- constant
    mARR "a" max
    mDEF "alen"
    mARR "b" max
    mDEF "blen"

    mARR "t" max
    mDEF "tlen"
    let a = arr "a"; b = arr "b"; t = arr "t"
    let alen = var "alen"; blen = var "blen"; tlen = var "tlen"

    mDEF "i"
    let i = var "i"
    {- init -}
    mFOR(i ^= val 0, i ?< val max, inc i) $ mDO $ do
        a ?! i ?= val 0
        b ?! i ?= val 0
        t ?! i ?= val 0

    a ?! val 0 ?= val 1     -- fib1 = 1
    alen ?= val 1
    b ?! val 0 ?= val 1     -- fib2 = 1
    blen ?= val 1

    mFOR(i ^= val 1, i ?< var "n", inc i) $ mDO $ do
        mDEF "carry"
        mDEF "maxlen"
        let carry = var "carry"; maxlen = var "maxlen"
        carry ?= val 0
        mIF(alen ?< blen)
            (maxlen ^= blen)
            (maxlen ^= alen)
        
        mDEF "j"
        let j = var "j"
        mFOR(j ^= val 0, j ?< maxlen, inc j) $ mDO $ do
            mDEF "a[j]+b[j]"
            var "a[j]+b[j]" ?= a ?! j ?+ b ?! j
            t ?! j ?= var "a[j]+b[j]" ?+ carry
            mIF(t ?! j ?< val 10)
                (carry ^= val 0)
                (mDO $ do
                    carry ?= val 1
                    t ?! j ?= t ?! j ?+ val (-10))
            
        mIF carry
            (mDO $ do
                t ?! j ?= carry
                mINC j)
            nop

        tlen ?= j

        mFOR(j ^= val 0, j ?< blen, inc j)
            (a ?! j ^= b ?! j)
        alen ?= blen

        mFOR(j ^= val 0, j ?< tlen, inc j)
            (b ?! j ^= t ?! j)
        blen ?= tlen

    mDEF "tmp"
    mFOR(i ^= alen ?- val 1, val 0 ?<= i, i ^= i ?- val 1) $ mDO $ do
      var "tmp" ?= a ?! i
      var "tmp" ?= var "tmp" ?+ chr '0'
      res ?<- var "tmp"
      res ?= res ?+ val 4
    res ?<- chr '\0'
    mRET $ val 0
      
