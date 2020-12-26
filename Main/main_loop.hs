import MipsGen.Monadic
import Main.Fact
import Main.String
import Main.System
import Data.Char ( ord ) 

includes :: IO String
includes = do
  f1 <- readFile ".\\Main\\fact.asm"
  return f1 
  return ""

definitions :: StmtM
definitions = do
  fact
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


mainLoop :: Stmt
mainLoop = 
  mDO $ do
    definitions
    mMACRO "main:\n"
    staticStr
    -- wstrInit "CMD:hello" "hello"
    -- mARR "str" 20
    mDEF "res"
    let res = var "res"

    -- var "res" ?= call "numToStr" [ref $ var "str", val 0]
    -- mDEF "res"
    mARR "str1" 20
    mARR "str2" 20
    mDEF "sptr"
    
    -- res ?= call "toSTR" [ref $ var "CMD:hello", ref $ var "str2"]

    -- var "sptr" ?= ref $ var "str2"
    -- reg "a0" ?= var "sptr"
    -- mMACRO "\tjal putsl\n"

    -- res ?= call "numToStr" [ref $ var "str1", val 114514]
    
    -- res ?= call "toSTR" [ref $ var "str1", ref $ var "str2"]

    -- var "sptr" ?= ref $ var "str2"
    -- reg "a0" ?= var "sptr"
    -- mMACRO "\tjal putsl\n"

    wstrInit "str1" "114514"
    res ?= call "sReadNum" [ref $ var "str1"]

    -- var "res" ?= call "stringCmp" [ref $ var "str1", ref $ var "str2"]
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
            res ?= call "fact" [res]
            res ?= call "numToStr" [ref $ var "wcmd", res]
            res ?= call "toSTR" [ref $ var "wcmd", ref $ var "cmd"]
            {- print res -}
            var "sptr" ?= ref $ var "cmd"
            reg "a0" ?= var "sptr"
            mMACRO "\tjal putsl\n"
          )
          (mDO $ do
            var "sptr" ?= ref $ var "STR:invalid"
            reg "a0" ?= var "sptr"
            mMACRO "\tjal putsl\n"
          )
        )
      

      mNOP

main :: IO ()
main = do
  let mainCode = runCompile Main mainLoop
  includeCode <- includes
  writeFile ".\\Main\\main_loop.asm" $ mainCode ++ includeCode
    