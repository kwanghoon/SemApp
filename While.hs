module While where

import Data.List

-- Syntax of Numerals (Page 10)
-- n ::= 0 | 1 | n 0 | n 1

data N = Z | O | NZ N | NO N  {- data constructor Z, O, NZ, and NO -}
       deriving Show
                
-- e.g., 101
n101 = NO (NZ O)


-- Semantic function for Numerals (Page 10)
-- N : Num -> Z
semN :: N -> Integer
semN Z      = 0              -- N[[0]] = 0
semN O      = 1              -- N[[1]] = 1
semN (NZ n) = 2 * semN n     -- N[[n 0]] = 2 . N[[n]
semN (NO n) = 2 * semN n + 1 -- N[[n 0]] = 2 . N[[n] + 1

-- State (Page 13)
-- State = Var -> Z
type State = [ (String,Integer) ]

-- E.g., [x|->5,y|->7,z|->0]
s0 = [ ("x",5), ("y",7), ("z",0) ]

-- State lookup

slookup :: State -> String -> Integer
slookup s x = head [i | (v, i) <- s, v == x]

-- s0 x = 5
-- [ i | (v, i) <- [ ("x",5), ("y",7), ("z",0) ], v == z]

-- Syntax of Arithmetic Expressions (Page 7)
-- a ::= n | x | a1 + a2 | a1 * a2 | a1 - a2 

data A = ANum N | AVar String | APlus A A | AMul A A | AMinus A A
       deriving Show
                
-- e.g., x + 1                
axp1 = APlus (AVar "x") (ANum O)

-- Semantic Function for Arithmetic Expressions (Page 14)
-- A : Aexp -> (State -> Z)
semA :: A -> (State -> Integer)
semA (ANum n) s       = semN n                 -- A[[n]] s = N[[n]]
semA (AVar x) s       = slookup s x            -- A[[x]] s = s x
semA (APlus a1 a2) s  = semA a1 s + semA a2 s  -- A[[a1 + a2]] s = A[[a1]] s + A [[a2]] s
semA (AMul a1 a2) s   = semA a1 s * semA a2 s  -- A[[a1 * a2]] s = A[[a1]] s * A [[a2]] s
semA (AMinus a1 a2) s = semA a1 s - semA a2 s  -- A[[a1 - a2]] s = A[[a1]] s - A [[a2]] s


-- Syntax of Boolean Expressions (Page 7)
-- b ::= true | false | a1 = a2 | a1 <= a1 | ~b | b1 /\ b2
data B = BTrue | BFalse | BEq A A | BLe A A | BNot B | BAnd B B
       deriving Show
                
-- e.g., ~(x = 1)                
b1 = BNot (BEq (AVar "x") (ANum O))  -- Caution!!! BNot (BEq (AVar "x") O)

-- Semantic Function for Boolean Expressions (Page 15)
-- B : Bexp -> (State -> T)
semB :: B -> (State -> Bool)
semB BTrue s = True   -- B[[true]] s = tt
semB BFalse s = False   -- B[[false]] s = ff
semB (BEq a1 a2) s = 
  if semA a1 s == semA a2 s then True -- B[[a1 = a2]] s = tt if A[[a1]] s = A[[a2]] s
  else False                          --                  ff if A[[a1]] s != A[[a2]] s
semB (BLe a1 a2) s = 
  if semA a1 s <= semA a2 s then True -- B[[a1 <= a2]] s = tt if A[[a1]] s <= A[[a2]] s
  else False                          --                   ff if A[[a1]] s > A[[a2]] s
semB (BNot b) s = 
  if semB b s == False then True      -- B[[~b]] s = tt if B[[b]] s == ff
  else False                          --             ff if B[[b]] s == tt
semB (BAnd b1 b2) s = 
  if semB b1 s && semB b2 s then True -- B[[b1/\b2]] s = tt if B[[b1]] s==tt and B[[b2]] s==tt
  else False                          --                 ff otherwise
       
-- Syntax of While (Page 7)
-- S ::= x := a | skip | S1 ; S2 | if b then S1 else S2 | while b do S
data S = SAss String A | SSkip | SSeq S S | SIf B S S | SWhile B S
       deriving Show
                
-- E.g., y:=1; while ~(x=1) do (y:=y+1; x:=x-1)                
w0 = SSeq (SAss "y" (ANum O))
          (SWhile (BNot (BEq (AVar "x") (ANum O)))
           ( SSeq (SAss "y" (APlus (AVar "y") (ANum O))) 
                  (SAss "x" (AMinus (AVar "x") (ANum O))) ))
     

-- Free Variables for Arithmetic Expressions
-- FV : Aexp -> A set of variables
fvA :: A -> [String]
fvA (ANum n) = []                          -- FV(n) = {}
fvA (AVar x) = [x]                         -- FV(x) = {x}
fvA (APlus a1 a2) = fvA a1 `union` fvA a2  -- FV(a1+a2) = FV(a1) U FV (a2)
fvA (AMul a1 a2) = fvA a1 `union` fvA a2   -- FV(a1*a2) = FV(a1) U FV (a2)
fvA (AMinus a1 a2) = fvA a1 `union` fvA a2 -- FV(a1-a2) = FV(a1) U FV (a2)

-- e.g., x + y * x (Page 16)
a2 = APlus (AVar "x") (AMul (AVar "y") (AVar "x")) 

-- Free Variables for Boolean Expressions
-- FV : Bexp -> A set of variables
-- fvB :: B -> [String]
{- HOMEWORK -}


-- Substitution for Arithemetic Expressions
-- a [y|->a0]  ~~~~>   [|->] a y a0
-- [|->] : Aexp -> Var -> Aexp -> Aexp

-- E.g., (x+1)[x|->3] = 3+1     ~~~~>      [|->] (x+1) x 3 

subA :: A -> String -> A -> A
subA (ANum n) y a0       = ANum n                               -- n[y|->a0] = n
subA (AVar x) y a0       = if x == y then a0                    -- x[y|->a0] = a0   if x = y
                           else AVar x                          --           = x    otherwise
subA (APlus a1 a2) y a0  = APlus (subA a1 y a0) (subA a2 y a0)  -- a1+a2[y|->a0] = a1[y|->a0] + a2[y|->a0] 
subA (AMul a1 a2) y a0   = AMul (subA a1 y a0) (subA a2 y a0)   -- a1*a2[y|->a0] = a1[y|->a0] * a2[y|->a0] 
subA (AMinus a1 a2) y a0 = AMinus (subA a1 y a0) (subA a2 y a0) -- a1-a2[y|->a0] = a1[y|->a0] - a2[y|->a0] 

-- E.g. the example above
sub1 = subA (APlus (AVar "x") (ANum O)) "x" (ANum (NO O))

-- Substitution for Boolean Expressions
-- b [y|->a0]  ~~~~>   [|->] b y a0
-- [|->] : Bexp -> Var -> Bexp -> Bexp

-- E.g., ???     ~~~~>      ???

--subB :: B -> String -> A -> B
{- HOMEWORK -}


