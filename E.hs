module E
where

import RunDFA

e :: DFA
e = ([0,1,2,3,4,5,6,7], "abc", t, 0, [0,1,2,3,4,5,6,7])
    where
        t = [((0, 'b'), 1)
            ,((0, 'c'), 1)
            ,((0, 'a'), 4)
            ,((1, 'b'), 2)
            ,((1, 'c'), 2)
            ,((1, 'a'), 4)
            ,((2, 'b'), 3)
            ,((2, 'c'), 3)
            ,((2, 'a'), 4)
            ,((3, 'a'), 4)
            ,((4, 'b'), 5)
            ,((4, 'c'), 5)
            ,((5, 'b'), 6)
            ,((5, 'c'), 6)
            ,((6, 'b'), 7)
            ,((6, 'c'), 7)
            ,((7, 'a'), 4)
            ]
