import MipsGen.Monadic

main :: IO ()
main = do
  writeFile ".\\scripts\\cond.asm" $ runCompile Main $ 
    mDO $ do 
      mMACRO "main:\n"
      mDEF "a"
      mDEF "b"
      var "a" ?= val 10
      mIF(var "a" ?< val 5) 
        (var "b" ^= val 1)
        (var "b" ^= val 2)

      mDEF "f"
      var "f" ?= var "a" ?== val 10 ?&& var "b" ?== val 1

      mDEF "c"
      mIF(var "a" ?== val 10 ?&& var "b" ?== val 1) -- false
        (var "c" ^= chr 'T')
        (var "c" ^= chr 'F')

      mDEF "d"
      mIF(var "b" ?== val 1 ?&& var "a" ?== val 10) -- false
        (var "d" ^= chr 'T')
        (var "d" ^= chr 'F')
      