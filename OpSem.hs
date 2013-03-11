module OpSem where

-- Ch.2
import While

-- Natural Semantics for While in Table 2.1
semNS :: S -> State -> State    --  <S,s> -> s'
semNS (SAss x a) s = [(x, semA a s)] ++ s         --  <x:=a,s> -> s[x|->A[[a]]s]
semNS (SSkip) s = s                               --  <skip,s> -> s
semNS (SSeq s1 s2) s = s''                          --  <S1;S2,s> -> s''
  where  
    s'  = semNS s1 s     -- <S1,s> -> s'
    s'' = semNS s2 s'    -- <S2,s'> -> s''
    
{- HOMEWORK -}    
    
{- if True then 1 else 2 
-}
    


-- Structural Operational Semantics for While in Table 2.2
semSOS ::  S -> State -> State
semSOS stmt s = state
  where
    config = semSOS' stmt s
    state  = case config of
              PAIR stmt' s' -> semSOS stmt' s'
              STATE s'      -> s'

data ConfigSOS = 
    STATE State    -- s
  | PAIR S State   -- <S,s>
    deriving Show
    
ex1 = STATE [("x", 1)] 
ex2 = PAIR (SSeq SSkip SSkip) [("x", 1)]
  
semSOS' :: S -> State -> ConfigSOS
semSOS' (SAss x a) s = STATE ([(x, semA a s)] ++ s)  -- [assSOS] <x:=a,s> => s[x|->A[[a]]s]
semSOS' (SSkip) s = STATE s 
semSOS' (SSeq s1 s2) s = config2
  where config1 = semSOS' s1 s
        config2 = case config1 of
                   PAIR s1' s' -> PAIR (SSeq s1' s2) s'  -- [comp1SOS]
                   STATE s'    -> PAIR s2 s'             -- [comp2SOS]
semSOS' (SIf b s1 s2) s = 
  if semB b s
  then PAIR s1 s  -- B[[b]] s == tt 
  else PAIR s2 s  -- B[[b]] s == ff
       
semSOS' (SWhile b s0) s =        
  PAIR (SIf b (SSeq s0 (SWhile b s0)) SSkip) s
  