import MipsGen.MipsGen

main :: IO ()
main = do
  putStr $ runCompile $
        Block
          [ 
            --Reg "t0" ?<= Reg "t1",
            Define $ Var "a",
            IF (Var "a" ?<= Reg "t1") (
              Block [
                Define $ Var "b",
                IF (Var "b" ?<= Reg "t2") NOP NOP
              ]
            ) 
            (
              NOP
            ) 
            --Var "a" ?<= Reg "b"
          ]
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