import MipsGen.Monadic
import Main.Fact
import Main.Fib
import Main.Expr
import Main.String
import Main.System
import Data.Char ( ord ) 

definitions :: StmtM
definitions = do
  fib
  fact
  evalExpr
  stringLen
  stringCmp
  numToStr  
  sReadNum
  toSTR
  toWSTR
  includeSys

{- declear the static strings, 
  the data will be ready after booting -}
staticStr :: StmtM 
staticStr = do
  mARR "CMD:hello" 7      -- "hello"L
  mARR "STR:hello" 5      -- "Hello, world!"
  mARR "STR:invalid" 5    -- "Invalid Command!"
  mARR "CMD:fact" 6       -- "fact"L
  mARR "CMD:fib" 4        -- "fib"L
  mARR "CMD:eval" 6       -- "eval"L
  mARR "CMD:echo" 6       -- "echo"L
  mARR "CMD:led" 5        -- "led"L
  mARR "CMD:setfg" 6      -- "setfg"L
  mARR "CMD:setbg" 6      -- "setbg"L
  mARR "CMD:cls" 4        -- "cls"L
  
mainLoop :: Stmt
mainLoop = 
  mDO $ do
    definitions
    mMACRO "main:\n"
    staticStr

    mDEF "res"
    let res = var "res"

    mDEF "sptr"
    
    mARR "fib_resW" 200
    mARR "fib_resS" 50

    mWHILE(val 1) $ mDO $ do  -- main loop
      mMACRO "\tjal prompt\n"
      reg "a0" ?= val 10000
      mMACRO "\tjal sleep\n"

      {- read a command -}
      mARR "cmd" (64 `div` 4)   -- string for command
      var "sptr" ?= ref $ var "cmd" 
      reg "a0" ?= var "sptr"
      reg "a1" ?= val 63
      mMACRO "\tjal readstr\n"

      mARR "wcmd" 64            -- wstring command
      var "res" ?= call "toWSTR" [ref $ var "cmd", ref $ var "wcmd"]

      mIF(call "stringCmp" [ref $ var "wcmd", ref $ var "CMD:hello"])
        (mDO $ do
          var "sptr" ?= ref $ var "STR:hello"
          reg "a0" ?= var "sptr"
          mMACRO "\tjal putsl\n"
        )
        (_if(call "stringCmp" [ref $ var "wcmd", ref $ var "CMD:fact"])
          (mDO $ do
            {- read a number -}
            var "sptr" ?= ref $ var "cmd" 
            reg "a0" ?= var "sptr"
            reg "a1" ?= val 63
            mMACRO "\tjal readstr\n"
            res ?= call "toWSTR" [ref $ var "cmd", ref $ var "wcmd"]
            res ?= call "sReadNum" [ref $ var "wcmd"]
            {- eval fact -}
            res ?= call "fact" [res]
            res ?= call "numToStr" [ref $ var "wcmd", res]
            res ?= call "toSTR" [ref $ var "wcmd", ref $ var "cmd"]
            {- print res -}
            var "sptr" ?= ref $ var "cmd"
            reg "a0" ?= var "sptr"
            mMACRO "\tjal putsl\n"
          )
          (_if(call "stringCmp" [ref $ var "wcmd", ref $ var "CMD:fib"])
            (mDO $ do
              {- read a number -}
              var "sptr" ?= ref $ var "cmd" 
              reg "a0" ?= var "sptr"
              reg "a1" ?= val 63
              mMACRO "\tjal readstr\n"
              res ?= call "toWSTR" [ref $ var "cmd", ref $ var "wcmd"]
              res ?= call "sReadNum" [ref $ var "wcmd"]
              {- eval fib -}
              res ?= call "fib" [res, ref $ var "fib_resW"]
              {- print res -}
              res ?= call "toSTR" [ref $ var "fib_resW", ref $ var "fib_resS"]
              var "sptr" ?= ref $ var "fib_resS"
              reg "a0" ?= var "sptr"
              mMACRO "\tjal putsl\n"
            )
            (_if(call "stringCmp" [ref $ var "wcmd", ref $ var "CMD:eval"])
              (mDO $ do
                {- read an expr -}
                var "sptr" ?= ref $ var "cmd" 
                reg "a0" ?= var "sptr"
                reg "a1" ?= val 63
                mMACRO "\tjal readstr\n"
                res ?= call "toWSTR" [ref $ var "cmd", ref $ var "wcmd"]
                {- eval -}
                res ?= call "evalExpr" [ref $ var "wcmd"]
                res ?= call "numToStr" [ref $ var "wcmd", res]
                res ?= call "toSTR" [ref $ var "wcmd", ref $ var "cmd"]
                var "sptr" ?= ref $ var "cmd"
                reg "a0" ?= var "sptr"
                mMACRO "\tjal putsl\n"
              )
              (_if(call "stringCmp" [ref $ var "wcmd", ref $ var "CMD:echo"])
                (mDO $ do
                  var "sptr" ?= ref $ var "cmd" 
                  reg "a0" ?= var "sptr"
                  reg "a1" ?= val 63
                  mMACRO "\tjal readstr\n"
                  reg "a0" ?= var "sptr"
                  mMACRO "\tjal putsl\n"
                )
                (_if(call "stringCmp" [ref $ var "wcmd", ref $ var "CMD:led"])
                  (mDO $ do
                    reg "a0" ?= val 0
                    reg "a1" ?= val 1
                    mMACRO "\tjal set_led\n"
                    mDEF "i"
                    let i = var "i"
                    mFOR(i ^= val 0, i ?< val 100, inc i) $ mDO $ do
                      reg "a0" ?= val 10000
                      mMACRO "\tjal sleep\n"
                      mMACRO "\tjal shift_led\n"
                    reg "a0" ?= val 0
                    reg "a1" ?= val 0
                    mMACRO "\tjal set_led\n"
                  )
                  (_if(call "stringCmp" [ref $ var "wcmd", ref $ var "CMD:setfg"])
                    (mDO $ do
                      {- read a number -}
                      var "sptr" ?= ref $ var "cmd" 
                      reg "a0" ?= var "sptr"
                      reg "a1" ?= val 63
                      mMACRO "\tjal readstr\n"
                      res ?= call "toWSTR" [ref $ var "cmd", ref $ var "wcmd"]
                      res ?= call "sReadNum" [ref $ var "wcmd"]
                      reg "a0" ?= res
                      mMACRO "\tjal setfg\n"
                    )
                    (_if(call "stringCmp" [ref $ var "wcmd", ref $ var "CMD:setbg"])
                      (mDO $ do
                        {- read a number -}
                        var "sptr" ?= ref $ var "cmd" 
                        reg "a0" ?= var "sptr"
                        reg "a1" ?= val 63
                        mMACRO "\tjal readstr\n"
                        res ?= call "toWSTR" [ref $ var "cmd", ref $ var "wcmd"]
                        res ?= call "sReadNum" [ref $ var "wcmd"]
                        reg "a0" ?= res
                        mMACRO "\tjal setbg\n"
                      )
                      (_if(call "stringCmp" [ref $ var "wcmd", ref $ var "CMD:cls"])
                        (mDO $ mMACRO "\tjal cls\n")
                        {- invalid -}
                        (_if(call "stringLen" [ref $ var "wcmd"] ?!= val 0)
                          (mDO $ do
                            var "sptr" ?= ref $ var "STR:invalid"
                            reg "a0" ?= var "sptr"
                            mMACRO "\tjal putsl\n")
                          nop
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      
main :: IO ()
main = do
  let mainCode = runCompile Main mainLoop
  writeFile ".\\Main\\main_loop.asm" mainCode
    