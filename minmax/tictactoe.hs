------------------------------------------------------------------------------
--
--  MinMax.TicTacToe
--
------------------------------------------------------------------------------

module Sample.MinMax.TicTacToe
  ( test
  ) where

------------------------------------------------------------------------------
--  imports
--
--  Core instead of Prelude (see Game.hs)
------------------------------------------------------------------------------

import Core
import Sample.MinMax.Game

------------------------------------------------------------------------------
--  TTT - Tac-Tac-Toe Board state
--
--  swapped - Yes if the xs and os are swapped (only used to print TTT correctly)
--  xbits   - positions where X has been played (stored as bits in an Int)
--  obits   - positions where O has been played
------------------------------------------------------------------------------

data TTT = TTT { swapped :: Swapped, xbits :: Int, obits :: Int } deriving Eq

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

data Swapped = No | Yes deriving Eq

instance Zero       Swapped where zero = No
instance Invertable Swapped where
  invert No  = Yes
  invert Yes = No

------------------------------------------------------------------------------
--  Lose n - will lose in n moves
--  Draw   - will draw
--  Win  n - will win  in n moves
------------------------------------------------------------------------------

data Value = Lose Int | Draw | Win Int deriving Eq

instance IDoc Value where
  doc (Lose n) = [docf|Loss in $n moves|]
  doc (Draw  ) = [docf|Draw|]
  doc (Win  n) = [docf|Win in $n moves|]

instance Invertable Value where
  invert (Lose n) = Win  n
  invert (Draw  ) = Draw
  invert (Win  n) = Lose n

instance Successor Value where
  succ (Lose n) = Lose (n + 1)
  succ (Draw  ) = Draw
  succ (Win  n) = Win  (n + 1)

------------------------------------------------------------------------------
--  it's better to lose in more  moves
--  it's better to win  in fewer moves
------------------------------------------------------------------------------

instance Ord Value where
  compare (Lose n) (Lose n') = compare n n'
  compare (Lose n) (_      ) = Less
  compare (Draw  ) (Lose n') = Greater
  compare (Draw  ) (Draw   ) = Equal
  compare (Draw  ) (Win  n') = Less
  compare (Win  n) (Win  n') = compare n' n
  compare (Win  n) (_      ) = Greater

------------------------------------------------------------------------------
--  !!!! TTT could be parameterized over size with dependent types
--       but instead just use the global constant 'size'
------------------------------------------------------------------------------

size          :: Int = 3
positionCount :: Int = size * size
playerBits    :: Int = positionCount
playerMask    :: Int = 2 ^ playerBits - 1
totalStates   :: Int = fromIndex (maxBound @TTT)

-- positions - a List of the possible positions
--   where
--     range min max = [min .. (max - 1)]

positions :: List Int
positions = range 0 positionCount

------------------------------------------------------------------------------
--  each TTT state has a unique Int index
--
--  class Zero - types that have a zero element
------------------------------------------------------------------------------

instance Zero    TTT where zero = TTT zero zero zero
instance Bounded TTT where
  minBound = zero
  maxBound = TTT zero playerMask playerMask

instance IIndex TTT where
  toIndex   :: Int -> TTT
  fromIndex :: TTT -> Int

  toIndex   xos           = TTT zero (playerMask && xos) (playerMask && shiftR playerBits xos)
  fromIndex (TTT s xs os) = xs || shiftL playerBits os

------------------------------------------------------------------------------
--  A Tic-Tac-Toe Game
------------------------------------------------------------------------------

instance Game TTT Value where
  moves (TTT swapped xs os) | isWinning size xs = Left   <| Win  0
                            | isWinning size os = Left   <| Lose 0
                            | True              = result <| mapMaybe move positions
    where
      -- the xs and os are swapped after making a move
      -- for the AI the current player is always X

      move position = if testBit position (xs || os) then None else Some <| TTT (invert swapped) os (xs || bit position)

      -- if there are no moves return Draw

      result (Nil      ) = Left  <| Draw
      result (Cons x xs) = Right <| NonEmptyList x xs

  printState :: TTT -> Value -> IO ()
  printState s v =
    do
      print s
      printLine
      go (swapped s) v
      printLine
    where
      go (_  ) (Draw  ) = [printf|Draw|]
      go (No ) (Lose n) = [printf|X's turn: O will win in $n moves|]
      go (No ) (Win  n) = [printf|X's turn: X will win in $n moves|]
      go (Yes) (Lose n) = [printf|O's turn: X will win in $n moves|]
      go (Yes) (Win  n) = [printf|O's turn: O will win in $n moves|]

------------------------------------------------------------------------------
--  isWinning - is a state winning?
--
--  This should really be a generic function, but I hand coded it to save development time
--  The hand coded version has better performance in any case
------------------------------------------------------------------------------

isWinning size = if size == 2 then isWinning2 else isWinning3

isWinning2 :: Int -> Bool
isWinning2 n = isSubset 0b_11_00 n
            || isSubset 0b_00_11 n
            || isSubset 0b_10_10 n
            || isSubset 0b_01_01 n
            || isSubset 0b_10_01 n
            || isSubset 0b_01_10 n

isWinning3 :: Int -> Bool
isWinning3 n =  isSubset 0b_111_000_000 n
             || isSubset 0b_000_111_000 n
             || isSubset 0b_000_000_111 n
             || isSubset 0b_100_100_100 n
             || isSubset 0b_010_010_010 n
             || isSubset 0b_001_001_001 n
             || isSubset 0b_100_010_001 n
             || isSubset 0b_001_010_100 n

------------------------------------------------------------------------------
--  turn TTT into a printable document
------------------------------------------------------------------------------

instance IDoc TTT where
  doc (TTT No  xs os) = tttDoc xs os
  doc (TTT Yes xs os) = tttDoc os xs

tttDoc xs os = columnDoc <| intersperse divider <| map lineDoc <| chunk size <| fmap xoDoc positions
  where
    xoDoc position | testBit position xs = [docf| X |]
                   | testBit position os = [docf| O |]
                   | True                = [docf|   |]

    lineDoc :: List Doc -> Doc
    lineDoc = sepWithDoc [docf|||] << map doc

    divider = sepWithDoc [docf|+|] <| replicate size [docf|---|]

------------------------------------------------------------------------------
--
------------------------------------------------------------------------------

test :: IO ()
test = do
  --play (zero :: TTT) - Draw
  play (TTT No 1 2) -- X wins
