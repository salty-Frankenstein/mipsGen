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
  -- fact
  stringLen
  stringCmp
  -- numToStr  
  toSTR
  toWSTR
  includeSys

{- declear the static strings -}
staticStr :: StmtM 
staticStr = do
  return ()

mainLoop :: Stmt
mainLoop = 
  mDO $ do
    definitions

    mMACRO "main:\n"
    -- mARR "str" 20
    mDEF "res"
    let res = var "res"
    mARR "CMD:hello" 6
    wstrInit "CMD:hello" "hello"
    mARR "WSTR:hello" 15
    wstrInit "WSTR:hello" "Hello, world!"
    mARR "STR:hello" 4
    res ?= call "toSTR" [ref $ var "WSTR:hello", ref $ var "STR:hello"]
    mARR "WSTR:invalid" 20
    wstrInit "WSTR:invalid" "Invalid Command!"
    mARR "STR:invalid" 5
    res ?= call "toSTR" [ref $ var "WSTR:invalid", ref $ var "STR:invalid"]

    -- var "res" ?= call "numToStr" [ref $ var "str", val 0]
    -- mDEF "res"
    --mARR "str1" 20
    --mARR "str2" 20

    -- stringInit "str1" "abcdefghijk"
    -- stringInit "str2" "abdde"
    -- var "res" ?= call "toSTR" [ref $ var "str1", ref $ var "str2"]
    mDEF "sptr"

    -- var "sptr" ?= ref $ var "str2"
    -- reg "a0" ?= var "sptr"
    -- mMACRO "\tjal putsl\n"

    
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
        (mDO $ do
          var "sptr" ?= ref $ var "STR:invalid"
          reg "a0" ?= var "sptr"
          mMACRO "\tjal putsl\n"
        )
      

      mNOP

main :: IO ()
main = do
  let mainCode = runCompile Main mainLoop
  includeCode <- includes
  writeFile ".\\Main\\main_loop.asm" $ mainCode ++ includeCode
    