module Main.String where

import MipsGen.Monadic
import Data.Char (ord)

{- initialize a wstring with name -}
wstrInit :: String -> String -> StmtM
wstrInit name s = do
  let
      input = s ++ "\0"
      strInt = map ord input
      valList = map val strInt
      varList = map (\i -> arr name ?! val i) [0..]
      strInit = zipWith (?=) varList valList
  sequence_ strInit   -- init

charConst :: StmtM 
charConst = do
  mDEF "chr0" >> mDEF "chr9"
  var "chr0" ?= chr '0'
  var "chr9" ?= chr '9'

stringLen :: StmtM
stringLen = do
  mPROC "stringLen" ["strptr"] $ mDO $ do
    let strp = var "strptr"
    mDEF "cnt"
    let cnt = var "cnt"
    cnt ?= val 0
    mWHILE(deref strp ?!= chr '\0') $ mDO $ do
      mINC cnt
      strp ?= strp ?+ val 4
    mRET cnt

stringCmp :: StmtM
stringCmp = do
  mPROC "stringCmp" ["strptr1", "strptr2"] $ mDO $ do
    let strptr1 = var "strptr1"; strptr2 = var "strptr2"
        chr1 = deref strptr1; chr2 = deref strptr2

    mDEF "len1" >> mDEF "len2"
    let len1 = var "len1"; len2 = var "len2"
    len1 ?= call "stringLen" [strptr1]
    len2 ?= call "stringLen" [strptr2]
    mIF(len1 ?!= len2)
      (_return $ val 0)
      (_while(chr1 ?!= chr '\0') $ mDO $ do
        mIF(chr1 ?!= chr2)
          (_return $ val 0)
          nop        
        strptr1 ?= strptr1 ?+ val 4
        strptr2 ?= strptr2 ?+ val 4
      )
    mRET $ val 1  -- return true

-- softDiv :: StmtM 
-- softDiv = do
--   mPROC "div" ["a", "b"] $ mDO $ do
--     mDEF "cnt"
--     let a = var "a"; b = var "b"; cnt = var "cnt"
--     cnt ?= val 0
--     mWHILE(val 0 ?< a) $ mDO $ do

-- string to wstring
toWSTR :: StmtM 
toWSTR = do
  mPROC "toWSTR" ["strptr", "resptr"] $ mDO $ do
    let strp = var "strptr"; resp = var "resptr"
    mDEF "ch"
    reg "t0" ?= strp
    mMACRO "\tlb $s0, ($t0)\n"
    var "ch" ?= reg "s0"
    mWHILE(var "ch" ?!= chr '\0') $ mDO $ do
      resp ?<- var "ch"

      resp ?= resp ?+ val 4
      strp ?= strp ?+ val 1
      reg "t0" ?= strp
      mMACRO "\tlb $s0, ($t0)\n"
      var "ch" ?= reg "s0"
    resp ?<- chr '\0'
    mRET $ val 0

-- wstring to string
toSTR :: StmtM 
toSTR = do
  mPROC "toSTR" ["strptr", "resptr"] $ mDO $ do
    let strp = var "strptr"; resp = var "resptr"
        sc = deref strp 
    mWHILE(sc ?!= chr '\0') $ mDO $ do
      mDEF "t"
      var "t" ?= sc
      reg "s0" ?= var "t"
      reg "t0" ?= resp
      mMACRO "\tsb $s0, ($t0)\n"
      resp ?= resp ?+ val 1
      strp ?= strp ?+ val 4
    
    reg "s0" ?= chr '\0'    -- end
    reg "t0" ?= resp
    mMACRO "\tsb $s0, ($t0)\n"

    mRET $ val 0

-- return the length of the result string
numToStr :: StmtM 
numToStr = do
  mPROC "numToStr" ["strptr", "n"] $ mDO $ do
    let strp = var "strptr"; n = var "n"
    mIF(n ?== val 0)
      (mDO $ do
        strp ?<- chr '0'
        mRET $ val 1
      )
      (mDO $ do
        mARR "tmp" 15
        mDEF "cnt" >> mDEF "d" >> mDEF "m" >> mDEF "n'"
        let tmp = arr "tmp"; cnt = var "cnt"
            d = var "d"; m = var "m"; n' = var "n'"
        cnt ?= val 0
        mWHILE(val 0 ?< n) $ mDO $ do
          d ?= n ?/ val 10        -- div
          n' ?= d ?* val 10
          m ?= n ?- n'   -- mod
          tmp ?! cnt ?= m ?+ chr '0'         -- save to tmp[cnt]
          mINC cnt
          n ?= d
        
        cnt ?= cnt ?- val 1
        mDEF "ret"
        var "ret" ?= cnt
        mWHILE(val 0 ?<= cnt) $ mDO $ do
          strp ?<- tmp ?! cnt
          strp ?= strp ?+ val 4
          cnt ?= cnt ?- val 1
        mRET $ var "ret"
      )

-- sReadNum ::
