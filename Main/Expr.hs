module Main.Expr where
import MipsGen.Monadic
import Data.Char (ord)

charConst :: StmtM 
charConst = do
  mDEF "chr0" >> mDEF "chr9"
  var "chr0" ?= chr '0'
  var "chr9" ?= chr '9'

evalExpr :: StmtM 
evalExpr = do
  mPROC "evalExpr" ["exprptr"] $ mDO $ do
    let max = 31   -- constant
    mARR "expr" max
    {- copy the expr -}
    mDEF "c" >> mDEF "cnt"
    var "c" ?= deref $ var "exprptr"
    var "cnt" ?= val 0
    mWHILE(var "c" ?!= chr '\0') $ mDO $ do
      arr "expr" ?! var "cnt" ?= var "c"

      mINC $ var "cnt"
      var "exprptr" ?= var "exprptr" ?+ val 4
      var "c" ?= deref $ var "exprptr"

    arr "expr" ?! var "cnt" ?= chr '\0'

    mDEF "res"  -- for result
    mARR "post" max  -- for post expression
    mDEF "post_ed"

    mARR "op_stack" max   -- operator stack
    mDEF "op_ed"

    mARR "num_stack" max  -- number stack
    mDEF "num_ed"

    charConst -- some char constants
    mDEF "i"
    let expr = arr "expr"
        post = arr "post"; post_ed = var "post_ed"
        op_stack = arr "op_stack"; op_ed = var "op_ed"
        num_stack = arr "num_stack"; num_ed = var "num_ed"
        i = var "i"; chr0 = var "chr0"; chr9 = var "chr9"

    {- mid to post expr -}
    op_ed ?= val 0
    post_ed ?= val 0

    mFOR(i ^= val 0, expr ?! i ?!= chr '\0', inc i) $ mDO $ do
      mIF(chr0 ?<= expr ?! i ?&& expr ?! i ?<= chr9)
        (mDO $ do -- then
          post ?! post_ed ?= expr ?! i
          mINC post_ed)
        (mDO $ do -- else
          mIF(expr ?! i ?== chr '(')
            (mDO $ do
              op_stack ?! op_ed ?= expr ?! i
              mINC op_ed)
            (_if(expr ?! i ?== chr ')')
              (mDO $ do 
                mWHILE(op_stack ?! (op_ed ?- val 1) ?!= chr '(') $ mDO $ do
                  post ?! post_ed ?= op_stack ?! (op_ed ?- val 1)
                  mINC post_ed
                  op_ed ?= op_ed ?- val 1
                op_ed ?= op_ed ?- val 1
              )
              (_if(expr ?! i ?== chr '*' ?&& 
                  op_stack ?! (op_ed ?- val 1) ?!= chr '*')
                (mDO $ do
                  op_stack ?! op_ed ?= expr ?! i
                  mINC op_ed)
                (mDO $ do 
                  mIF(expr ?! i ?== chr '*')
                    (_while(op_ed ?!= val 0 ?&& 
                        op_stack ?! (op_ed ?- val 1) ?== chr '*') $ mDO $ do 
                      post ?! post_ed ?= op_stack ?! (op_ed ?- val 1)
                      mINC post_ed
                      op_ed ?= op_ed ?- val 1
                    )
                    (_while(op_ed ?!= val 0 ?&&
                        op_stack ?! (op_ed ?- val 1) ?!= chr '(') $ mDO $ do
                      post ?! post_ed ?= op_stack ?! (op_ed ?- val 1)
                      mINC post_ed
                      op_ed ?= op_ed ?- val 1
                    )
                  op_stack ?! op_ed ?= expr ?! i
                  mINC op_ed
                )
              )
            )
          )

    mWHILE(op_ed ?!= val 0) $ mDO $ do
      post ?! post_ed ?= op_stack ?! (op_ed ?- val 1)
      mINC post_ed
      op_ed ?= op_ed ?- val 1
    
    {- eval post expr -}
    num_ed ?= val 0

    mFOR(i ^= val 0, i ?< post_ed, inc i)
      (_if(chr0 ?<= post ?! i ?&& post ?! i ?<= chr9)
        (mDO $ do
          num_stack ?! num_ed ?= post ?! i ?- chr '0'
          mINC num_ed
        )
        (mDO $ do
          mDEF "left" >> mDEF "right"
          let left = var "left"; right = var "right"
          right ?= num_stack ?! (num_ed ?- val 1)
          num_ed ?= num_ed ?- val 1
          left ?= num_stack ?! (num_ed ?- val 1)
          num_ed ?= num_ed ?- val 1
          mIF(post ?! i ?== chr '+')
            (mDO $ do
              num_stack ?! num_ed ?= left ?+ right
              mINC num_ed
            )
            (_if(post ?! i ?== chr '-')
              (mDO $ do
                num_stack ?! num_ed ?= left ?- right
                mINC num_ed
              ) 
              (_if(post ?! i ?== chr '*')
                (mDO $ do
                  num_stack ?! num_ed ?= left ?* right
                  mINC num_ed
                )
                nop
              )
            )

        )
      )

    var "res" ?= num_stack ?! (num_ed ?+ val (-1))
    mRET $ var "res"
      