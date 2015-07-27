module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend pre x v = \y -> if x == y then v else pre y

empty :: State
empty = \_ -> 0

-- Exercise 2 -----------------------------------------

doDop :: State -> Bop -> Expression -> Expression -> Int
doDop s b e1 e2 = case b of
  Plus   -> v1 + v2
  Minus  -> v1 - v2
  Times  -> v1 * v2
  Divide -> v1 `div` v2
  Gt     -> toN $ v1 > v2
  Ge     -> toN $ v2 >= v2
  Lt     -> toN $ v1 < v2
  Le     -> toN $ v1 <= v2
  Eql    -> toN $ v1 == v2
  where v1    = evalE s e1
        v2    = evalE s e2
        toN x = if x == True then 1 else 0

evalE :: State -> Expression -> Int
evalE _ (Val x) = x
evalE s (Var x) = s x
evalE s (Op e1 dop e2) = doDop s dop e1 e2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar s = case s of
  (Assign s e)     -> DAssign s e
  (Incr s)         -> DAssign s (Op (Var s) Plus (Val 1))
  (If e s1 s2)     -> DIf e (desugar s1) (desugar s2)
  (While e s)      -> DWhile e (desugar s)
  (For s1 e s2 s3) -> DSequence (desugar s1) (DWhile e (DSequence (desugar s3) (desugar s2) ) )
  (Sequence s1 s2) -> DSequence (desugar s1) (desugar s2)
  (Skip)           -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state ds = case ds of
  (DAssign s e)     -> extend state s (evalE state e)
  (DIf e s1 s2)     -> if evalE state e == 1 then evalSimple state s1 else evalSimple state s2
  (DWhile e s)      -> if evalE state e == 1 then evalSimple (evalSimple state s) ds else state
  (DSequence s1 s2) -> evalSimple (evalSimple state s1) s2
  (DSkip)           -> state

run :: State -> Statement -> State
run s st = evalSimple s (desugar st) 

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
