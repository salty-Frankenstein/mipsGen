module Main.Fact where

import MipsGen.Monadic

fact :: StmtM
fact = do
  mPROC "fact" ["n"] $ mDO $ do
    mIF(var "n" ?== val 0)
      (_return $ val 1) 
      (mDO $ do
        mDEF "t"
        var "t" ?= call "fact" [var "n" ?- val 1]
        mRET $ var "n" ?* var "t")
    