------------------------------------------------------------------------------
--  types
------------------------------------------------------------------------------

type X             = String
type Operator      = X
data E             = EX X | EApply E Operator E
data Associativity = LeftAssociative | RightAssociative
data Precidence    = Precidence Associativity Int

------------------------------------------------------------------------------
--  Precidence
------------------------------------------------------------------------------

data Associativity = LeftAssociative | RightAssociative
data Precidence    = Precidence Associativity Int

instance Zero Precidence where zero = Precidence LeftAssociative 0

higher :: Precidence -> Precidence -> Bool
higher (Precidence _ x) (Precidence LeftAssociative  y) = y >  x
higher (Precidence _ x) (Precidence RightAssociative y) = y >= x

------------------------------------------------------------------------------
--  algorithm
------------------------------------------------------------------------------

delimit :: (Operator -> Precidence) -> [X] -> E
delimit pop []       = EX "<error>"
delimit pop (x : xs) = first (go zero (EX x) xs)
  where
    go :: Precidence -> E -> [X] -> (E, [X])
    go pLeft eLeft (op : x : xs) | higher pLeft (pop op) = let (eRight, xs') = go (pop op) (EX x) xs in go pLeft (EApply eLeft op eRight) xs'
    go pLeft eLeft xs                                    = (eLeft, xs)

------------------------------------------------------------------------------
--  usage
------------------------------------------------------------------------------

ops :: Map X Precidence
ops = toT ( [ ("+", Precidence LeftAssociative  1)
            , ("*", Precidence LeftAssociative  2)
            , ("^", Precidence RightAssociative 3)
            ] :: List (X, Precidence)
          )

e = delimit (lookupDefault zero ops) [ "x", "+", "y", "^", "z", "^", "m", "*", "w", "+", "n" ]
