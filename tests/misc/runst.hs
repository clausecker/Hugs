--!!! Testing typechecking of runST
module RunSTTest where

import ST

t1 = runST (return '1')

t2 = runST (do
       v <- newVar '2'
       readVar v
       )

