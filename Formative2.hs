module Formative2 where

{-
# SOF3/Formative 2: The Royal Game of Ur

This formative exercise asks you to implement components for playing
***The Royal Game of Ur***.

Throughout this exercise you may find it useful to implement utility
functions that are not explicitely requested, as well as those that
are requested.

We have provided a few tests to
1. clarify what the functions are to do.
2. help you test your solutions.

Note that passing these tests is _not_ a guarantee that you have got
your definitions correct in any other cases.  You are encouraged to
test further.

The Royal Game of Ur is the oldest known board game.  The oldest known
board and pieces are around 4,500 years old.  The only rule book we
have, which is for versions of the game rather than the original game,
is a youthful 2,500 years or so old.  This rule book is written in
cuneiform letters on a clay tablet, and currently resides in the
British Museum.  It was translated by one of the British Museum's
curators, Irving Finkel.  There are YouTube videos of him
[discussing](https://youtu.be/wHjznvH54Cw) and [playing the game with
Tom Scott](https://youtu.be/WZskjLq040I) (Tom Scott is a linguistics
graduate from the University of York, ex-YUSU president, and has a
popular YouTube channel.).

The rules are not completely known, but this exercise uses rules based
on the [Finkel-Scott match](https://youtu.be/WZskjLq040I).

The game is a race game, such as ludo or backgammon, for two players,
that we will call `Red` and `Green`.
-}
data Player = Red | Green deriving (Eq, Show)

{-
A board contains 14 _logical_ squares, arranged as 20 _physical_ squares.

```
-----------------------------------------                   ---------------------
| *Sq_4   |  Sq_3   |  Sq_2   |  Sq_1   |                   | *Sq14   |  Sq13   |
---------------------------------------------------------------------------------
|  Sq_5   |  Sq_6   |  Sq_7   | *Sq_8   |  Sq_9   |  Sq10   |  Sq11   |  Sq12   |
---------------------------------------------------------------------------------
| *Sq_4   |  Sq_3   |  Sq_2   |  Sq_1   |                   | *Sq14   |  Sq13   |
-----------------------------------------                   ---------------------
```

In addition there are two "locations" off of the board:
* the start, where pieces wait to move onto the board
* home, where pieces are when they have finished their journey around the board.

-}

data Position = Start -- off board
              | Sq_1 | Sq_2 | Sq_3 | Sq_4 -- private
              | Sq_5 | Sq_6 | Sq_7 | Sq_8 | Sq_9 | Sq10 | Sq11 | Sq12 -- shared
              | Sq13 | Sq14 -- private
              | Home -- off board
              deriving -- type classes that may be useful in your solution
                (Eq, Ord, Enum, Bounded, Show)

{-
The red player's pieces move along the top and middle rows, in
numerical order, while the green player's pieces move similarly along
the bottom and middle rows.  Squares 1-4 and 13-14 are private, but
squares 5-12 are _shared_ and where, in Irving Finkel's words, the two
players are "at war".

There are special squares: the private squares 4 and 14, and the
shared square 8.  On a real board these are decorated with a rosette,
indicated above by an asterisk ('*').
-}

{-
Each player has seven (7) identical pieces.  Each piece has a
position:
* waiting to enter the board (at the `Start`),
* on a square on the board, or
* having reached `Home`.

-}
piecesPerPlayer :: Int
piecesPerPlayer = 7

{-
The dice used are essentially four tossed coins that could be 1
(heads) or 0 (tails) each, and the value of the "throw" is the sum.

This gives probabilities:
* 0 → 1/16
* 1 → 4/16 = 1/4
* 2 → 6/16 = 3/8
* 3 → 4/16 = 1/4
* 4 → 1/16

You are not asked to implement the dice: when rolls are needed these
will be provided by an external oracle.
-}

{-
We want to know where pieces are on the board.  To do this we use a
function which answers the question.
-}
type Placement = (Position, Player) -> Int
{-
If `p :: Placement` then the expression `p (Sq_3, Red)` is the number of
pieces that `Red` has in position `Sq_3`.  Similarly, `p (Home, Green)` is
the number of pieces that `Green` has that have reached `Home`.

The state of a game is a `Placement` and the `Player` whose turn it is.

-}
data GameState = GameState Placement Player
{-
---

## Q1 [2 marks]

Implement the utility function `opponent` that returns a player's opponent.
-}
opponent :: Player -> Player
opponent Red = Green
opponent Green = Red
test_opponent :: Bool
test_opponent = opponent Red == Green
{-
## Q2 [2 marks]

Implement the utility function `isValidRoll` that checks a dice roll for being in range.

-}
isValidRoll :: Int -> Bool
isValidRoll = undefined
test_isValidRoll :: Bool
test_isValidRoll  = isValidRoll 2 && not (isValidRoll 9)

{-
## Q3 [2 marks]

Implement the utility function `plus` that adds a dice roll to a
position to get a new position (values that go beyond `Home` should be
treated as `Home`).

-}
plus :: Position -> Int -> Position
plus x roll | fromEnum x + roll > 14 = Home
            | otherwise = toEnum (fromEnum x + roll) :: Position

test_plus :: Bool
test_plus =    Start `plus` 0 == Start
            && Start `plus` 3 == Sq_3
            && Sq_3  `plus` 2 == Sq_5
            && Sq14  `plus` 4 == Home
{-

## Q4 [4 marks]

Implement a pair of functions, `fromList` and `toList` that convert
between lists and `Placement`s.  They should be inverses of each other.
-}
toList :: Placement -> [((Position, Player), Int)]
fromList :: [((Position, Player), Int)] -> Placement

toList p = [((pos,pla),p (pos,pla))| pos <- [Start .. Home], pla <- [Green,Red]]



fromList ps = placement
    where
      placement (pos,pla) = maybe 0 id $ lookup (pos,pla) ps

x = 2


testToFromList :: Bool
testToFromList = ((Sq_3, Red), 9) `elem` toList(fromList [((Sq_3, Red), 9)])
                 && ((Sq10, Red), 0) `elem` toList(fromList [((Sq_3, Red), 9)])
{-
We can now instantiate `GameState` as an instance of `Eq`:

-}
instance Eq GameState where
  GameState p x == GameState q y = (toList p, x) == (toList q, y)
{-

## Q5 [4 marks]

The pair of functions `toList`/`fromList` can take inputs that do not
represent a valid placing of pieces: for example, there may not be the
right number of tokens for `Red` or `Green`, or there may be too many
peices associated with a position.

Implement a pair of functions `validList` and `validPlacement` that
check for validity.

-}

validPlacement :: Placement -> Bool
validList :: [((Position, Player), Int)] -> Bool
validList xs = (check extractGreen) && (check extractRed) && checkAll
  where extractGreen = [(val)| ((pos, pl), val) <- xs, pl == Green]
        extractRed = [(val)| ((pos, pl), val) <- xs, pl == Red]
        check ys = (sum ys) == piecesPerPlayer
        checkAll = all (\((x, y), z) -> z <= piecesPerPlayer ) xs
validPlacement = validList . toList

test_validPlacement :: Bool
test_validPlacement = not (validPlacement (fromList [((Start,Red),9), ((Start,Green),7)]))
                      && not (validPlacement (fromList [((Start,Red),6), ((Start,Green),7)]))
{-
## Q6 [2 marks]

The initial state has both players with all their tokens at the start.
The first move belongs to the "red" player.  Implement this state.
-}
initGS :: GameState
initGS = GameState (fromList [((Start, Green), 7), ((Start, Red), 7)]) Red

test_initGS_placement :: Bool
test_initGS_placement = validPlacement plac
                        && plac (Start, Red) == 7
                        && plac (Sq10, Green) == 0
                        && plac (Home, Red) == 0
                        && rd == Red
  where
    GameState plac rd = initGS

{-
## Q7 [6 marks]
A move from a chosen position by the current player is possible if:
1. The chosen position is not `Home`.
2. There is a piece belonging to the current player on the chosen position.
3. There is not already a piece belonging to the current player in the
   new position, unless the new position is `Home`.
4. The new position is not the shared rosette currently occupied by
   the other player.

Implement the function `possibleMoves` that returns all the squares
from which the current player has a possible move.  The function may
assume that the input dice roll is in the valid range.  When the roll
is `0` there is an ambiguity: are there no moves, or can any piece be
"moved" to the square it is already on?  You should report no possible
moves.
-}
possibleMoves :: GameState -> Int -> [Position]
possibleMoves _ 0 = []
possibleMoves (GameState placement player) roll = [x | x <-  [Start .. Sq14],
                                                  (placement (x, player))==1,
                                                  ((plus x roll) /= Sq_8 || (placement(Sq_8, opponent player)==0)),
                                                  placement(plus x roll, player) == 0, ]

test_possibleMoves :: Bool
test_possibleMoves =    possibleMoves initGS 0 == []
                     && possibleMoves initGS 3 == [Start]

{-
## Q8 [8 marks]

Now implement a function `move` that takes a `GameState` and a
dice-roll/position-to-move-from pair and returns a new `GameState`.
1. If any input is invalid, (an invalid dice roll, or the roll is
   valid but it is not possible to move that distance from the
   nominated position) then the game state does not change.
2. If the current dice roll is valid, then
   1. If there are valid moves with the dice roll:
      1. The current player chooses one.
      2. The player's token is moved from the chosen position to the new
         position.
      3. If the new position is a shared square, and it is occupied by
         the other player then the other player's piece returns to the
         start.
      4. The next player is the other player, unless the new position is
       a rosette.
   2. If the dice roll has no valid moves, the placement does not
       change, but the next player is the other player

-}

move :: GameState -> (Int, Position) -> GameState
move = undefined

test_move :: Bool
test_move =    plac1 (Start, Red) == pred piecesPerPlayer
            && plac1 (Sq_1, Red) == 1
            && plac1 (Sq_2, Red) == 0
            && plac1 (Start, Green) == piecesPerPlayer
            && plac1 (Sq_1, Green) == 0
            && plr1 == Green
            && plac2 (Start, Red) == pred piecesPerPlayer
            && plac2 (Sq_1, Red) == 1
            && plac2 (Sq_2, Red) == 0
            && plac2 (Start, Green) == pred piecesPerPlayer
            && plac2 (Sq_1, Green) == 0
            && plac2 (Sq_2, Green) == 1
            && plr2 == Red
            && plac2' (Start, Green) == piecesPerPlayer
            && plac2' (Sq_1, Red) == 1
            && plac2' (Sq_2, Red) == 0
            && plac2' (Sq_2, Green) == 0
            && plr2' == Green
  where
    gs1 = move initGS (1, Start)
    GameState plac1 plr1 = gs1
    gs2 = move gs1 (2, Start)
    GameState plac2 plr2 = gs2
    gs2' = move gs1 (5, Start)
    GameState plac2' plr2' = gs2'

{-
## Q9 [4 marks]

Implement a function, `gameOver` to report if the game has been won or
not, and if won, then who the winner is.
-}
gameOver :: GameState -> Maybe Player
gameOver = undefined

test_gameOver :: Bool
test_gameOver =    gameOver initGS == Nothing
{-
Further tests are in Q10.
-}

{-
## Q10 [6 marks]

Implement a function, `playSequence`, that calculates the result of a
finite series of moves, starting from the initial state.
-}
playSequence :: [(Int, Position)] -> GameState
playSequence = undefined

test_playSequence_gameOver :: Bool
test_playSequence_gameOver =
     gameOver (playSequence [])                      == Nothing
  && gameOver (playSequence [(4, Start), (4, Sq_4)]) == Nothing
  && gameOver (playSequence (take 40 seq1))          == Nothing
  && gameOver (playSequence (take 41 seq1))          == Just Red
  && gameOver (playSequence (take 42 seq2))          == Just Green
  where
    seq1 = cycle [(4, Start), (4, Sq_4), (4, Sq_8), (0, Start), (4, Sq12), (0, Start)]
    seq2 = (0, Start) : seq1
