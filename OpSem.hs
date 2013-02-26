module OpSem where

-- Ch.2
import While

-- Natural Semantics for While in Table 2.1
semNS :: S -> State -> State    --  <S,s> -> s'
semNS (SAss x a) s = [(x, semA a s)] ++ s         --  <x:=a,s> -> s[x|->A[[a]]s]
semNS (SSkip) s = s                               --  <skip,s> -> s
semNS (SSeq s1 s2) = s''                          --  <S1;S2,s> -> s''
  where  
    s'  = semNS s1 s     -- <S1,s> -> s'
    s'' = semNS s2 s'    -- <S2,s'> -> s''
    
{- HOMEWORK -}    
    
{- if True then 1 else 2 -}
    

