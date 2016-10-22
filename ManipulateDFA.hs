module ManipulateDFA
where

import DFA
import Data.Char (isDigit, isLower)
import Data.List (sort, nub, (\\), intersect, elemIndex)
import Data.Maybe (fromJust)

{------------------------------------------------------------------------
    A Haskell script for COMP30026 Asg2, 2016.

    Skeleton code containing a bunch of stubs, mainly function type signatures
    is provided. (Actually the skeleton code can miraculously pass half the test
    cases. Definitively one of the weirder projects I've done)

    Authro: delsin
------------------------------------------------------------------------}


--  Keep lists sorted and without duplicates.

tidy :: Ord a => [a] -> [a]
tidy xs
  = nub (sort xs)


--  Calculate the set of reachable states in a given DFA.

reachable :: DFA -> [State]
reachable (states, alphabet, delta, start_state, accept_states)
  = new
    where
      (old, new) = until stable explore ([], [start_state])
      explore (old_reach, cur_reach) = (cur_reach, expand cur_reach)
      expand reach = tidy (reach ++ successors reach)
      successors reach = [y | ((x,_),y) <- delta, x `elem` reach]
      stable (xs, ys) = xs == ys


--  Calculate the set of generating states in a given DFA.

generating :: DFA -> [State]
generating (states, alphabet, delta, start_state, accept_states)
  = new
    where
        (old, new) = until stable explore ([], accept_states)
        explore (old_gen, cur_gen) = (cur_gen, expand cur_gen)
        expand gen = tidy (gen ++ predecessors gen)
        predecessors gen = [x | ((x,_),y) <- delta, y `elem` gen]
        stable (xs, ys) = xs == ys


--  Trim a DFA, that is, keep only reachable, generating states
--  (the start state should always be kept).

trim :: DFA -> DFA
trim (states, alphabet, delta, start_state, accept_states)
  = (trim_states, alphabet, trim_delta, start_state, trim_accept_states)
    where
        trim_states = tidy((intersect reachable_states generating_states) ++ [start_state])
        reachable_states = reachable (states, alphabet, delta, start_state, accept_states)
        generating_states = generating (states, alphabet, delta, start_state, accept_states)
        trim_delta = [((x,a),y) | ((x,a),y) <- delta, x `elem` trim_states && y `elem` trim_states]
        trim_accept_states = tidy(intersect trim_states accept_states)



-------------------------------------------------------------------------

--  Complete a DFA, that is, make all transitions explict.  For a DFA,
--  the transition function is always understood to be total.

complete :: DFA -> DFA
complete (states, alphabet, delta, start_state, accept_states)
  | is_complete
    = (states, alphabet, delta, start_state, accept_states)
  | otherwise
    = (complete_states, alphabet, complete_delta, start_state, accept_states)
    where
        all_trans = [(x, s) | x <- states, s <- alphabet]
        is_complete = (all_trans == [fst tran | tran <- delta])
        reject_state = maximum states + 1
        complete_states = states ++ [reject_state]
        complete_all_trans = [(x, s) | x <- complete_states, s <- alphabet]
        additional_delta = [(tran, reject_state) | tran <- complete_all_trans \\ all_trans]
        complete_delta = delta ++ additional_delta



-------------------------------------------------------------------------

--  Systematically replace the names of states in a DFA with 1..n.

normalise :: DFA -> DFA
normalise (states, alphabet, delta, start_state, accept_states)
  = (norm_states, alphabet, norm_delta, norm_start_state, norm_accept_states)
  where
      replace state = fromJust(elemIndex state states) + 1
      norm_states = [replace state | state <- states]
      norm_delta = [((replace x, s), replace y) | ((x, s), y) <- delta]
      norm_start_state = replace start_state
      norm_accept_states = [replace state | state <- accept_states]



-------------------------------------------------------------------------

--  To complete and then normalise a DFA:

full :: DFA -> DFA
full
  = normalise . complete


--  For a given DFA d, generate a DFA d' so that the languages of d
--  and d' are complementary.

complement :: DFA -> DFA
complement dfa
  = complete(swap (complete dfa))
  where
      swap (states, alphabet, delta, start_state, accept_states)
        = (states, alphabet, delta, start_state, states \\ accept_states)

-------------------------------------------------------------------------

--  Given DFAs d1 and d', generate a DFA for the intersection of the
--  languages recognised by d1 and d2.

prod :: DFA -> DFA -> DFA
prod dfa1 dfa2
  = complete(process full_dfa1 full_dfa2)
  where
      full_dfa1 = full dfa1
      full_dfa2 = full dfa2
      process (states0, alphabet0, delta0, start_state0, accept_states0) (states1, alphabet1, delta1, start_state1, accept_states1)
        = complete(tidy (states0 ++ states1), tidy (alphabet0 ++ alphabet1), tidy (delta0 ++ delta1), start_state0, intersect accept_states0 accept_states1)
-- Fuck this piece of shit code. How the hell did this pass all the test cases?
-- I don't even know why this worked.

-------------------------------------------------------------------------

--  Here is an example (trimmed) DFA; it recognises a*ab*c*

dex :: DFA
dex
  = ([0,1,2,3], "abc", t1, 0, [1,2,3])
    where
      t1 = [ ((0,'a'), 1)
           , ((1,'a'), 1)
           , ((1,'b'), 2)
           , ((1,'c'), 3)
           , ((2,'b'), 2)
           , ((2,'c'), 3)
           , ((3,'c'), 3)
           ]

-------------------------------------------------------------------------
