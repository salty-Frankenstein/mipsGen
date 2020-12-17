import MipsGen.Monadic

main :: IO ()
main = do
  writeFile "./out.asm" $ runCompile $ 
    mDO $ do 
      mDEF "a"
      mIF (var "a" ?<= reg "t1") 
        (mDO $ do
          mDEF "b"
          mIF (var "b" ?<= reg "t2") nop nop
        ) 
        (mDO $ do
          mIF (var "a" ?<= reg "t2") (mDO mNOP) (mDO mNOP)
        )
      mIF (var "a" ?<= reg "t1") nop nop
      mIF (var "a" ?<= reg "t1") nop nop

-- Block
--     [ Reg "t0" ?= Val 1,
--     Inc $ Reg "t0",
--     Define $ Var "a",
--     Inc $ Var "a",
--     Var "a" ?= Reg "t0",
--     Define $ Var "b",
--     Var "b" ?= Val 10,
--     Block
--         [ --Var "x" ?= Reg "t0",  -- error: undefined
--         Define $ Var "x",
--         Var "x" ?= Reg "t0"
--         ],
--     --Var "x" ?= Reg "t0" -- error: out of scope
--     Var "b" ?= Val 20
--     ]