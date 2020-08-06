------------------------------------------------------------------------------
--  AST
------------------------------------------------------------------------------

data E             = EX X | EApply E Operator E
type X             = String
type Operator      = X

------------------------------------------------------------------------------
--  Precedence
------------------------------------------------------------------------------

data Precedence    = Precedence Associativity Int
data Associativity = LeftAssociative | RightAssociative

zero = Precedence LeftAssociative 0

higher :: Precedence -> Precedence -> Bool
higher (Precedence _ x) (Precedence LeftAssociative  y) = y >  x
higher (Precedence _ x) (Precedence RightAssociative y) = y >= x

------------------------------------------------------------------------------
--  algorithm
------------------------------------------------------------------------------

delimit :: (Operator -> Precedence) -> [X] -> E
delimit pop []       = EX "<error>"
delimit pop (x : xs) = first (go zero (EX x) xs)
  where
    go :: Precedence -> E -> [X] -> (E, [X])
    go pLeft eLeft (op : x : xs) | higher pLeft (pop op) = let (eRight, xs') = go (pop op) (EX x) xs in go pLeft (EApply eLeft op eRight) xs'
    go pLeft eLeft xs                                    = (eLeft, xs)

------------------------------------------------------------------------------
--  usage
------------------------------------------------------------------------------

ops :: Map X Precedence
ops = toT ( [ ("+", Precedence LeftAssociative  1)
            , ("*", Precedence LeftAssociative  2)
            , ("^", Precedence RightAssociative 3)
            ] :: List (X, Precedence)
          )

e = delimit (lookupDefault zero ops) [ "x", "+", "y", "^", "z", "^", "m", "*", "w", "+", "n" ]
