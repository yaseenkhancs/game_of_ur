module Urrr where
import Data.List (foldl')
import Text.Read (readMaybe)

data Playr = Green | Red deriving (Show, Eq)

{-
14 logical squares, 20 physical squares
-------------------------------------------------------------
| *4   |  3   |  2   |  1   |              | *14   |  13   | 
-------------------------------------------------------------
|  5   |  6   |  7   | *8   |  9   |  10   |  11   |  12   | 
-------------------------------------------------------------
| *4   |  3   |  2   |  1   |              | *14   |  13   | 
-------------------------------------------------------------
-}

data Position = Start | Sqr_1 | Sqr_2 | Sqr_3 | Sqr_4 | Sqr_5 | Sqr_6 | Sqr_7 | Sqr_8 | Sqr_9 | Sqr10 | Sqr11 | Sqr12 | Sqr13 | Sqr14 | Home
              deriving
                (Show, Ord, Enum, Eq, Bounded)

{-
position utilities:
-}
notAtHome, isaRosette, isaSharedRosette, ititisShared :: Position -> Bool
notAtHome = (/= Home)
isaSharedRosette pos = isaRosette pos && itisShared pos
isaRosette = (`elem` [Sqr_4, Sqr_8, Sqr14])
itisShared pos = Sqr_5 <= pos && pos <= Sqr12

pieceperPlayr :: Int
pieceperPlayr = 7

-- more utilities
maximumRoll, minimumRoll  :: Int
maximumRoll = 4
minimumRoll = 0

--placement of pieces on the board:

type Placmnt = (Position, Playr) -> Int

--gamestate is a placement and the turn of the player

data GmState = GmState Placmnt Playr


--oppo returns player's opponent

oppo :: Playr -> Playr
oppo Red = Green
oppo Green = Red
tst_oppo = oppo Red == Green
tst_oppo :: Bool

--checks if a diceroll is in range

isRollValid :: Int -> Bool
isRollValid roll = minimumRoll <= roll && roll <= maximumRoll
tst_isRollValid :: Bool
tst_isRollValid  = isRollValid 2 && not (isRollValid 9)

--add dice roll to position to get new position

addDiceRoll :: Position -> Int -> Position
addDiceRoll posit aroll = min (toEnum (((fromEnum posit) + aroll)) (fromEnum Home)
tst_addDiceRoll :: Bool
tst_addDiceRoll = addDiceRoll Start 0 == Start && addDiceRoll Start 3 == Sqr_3 && addDiceRoll Sqr_3 2 == Sqr_5 && addDiceRoll Sqr14 4 == Home

--list to placement converters

toAList :: Placmnt -> [((Position, Playr), Int)]
fromAList :: [((Position, Playr), Int)] -> Placmnt
toAList plac = [(ppp, n) | posit <- [Start .. Home], pl <- [Red, Green], let ppp = (posit, pl), let n = plac ppp, n /= 0]
fromAList la = maybe 0 id . flip lookup la -- takes first match
testToFromAList :: Bool
testToFromAList = elem ((Sqr_3, Red), 9)  (toAList(fromAList [((Sqr_3, Red), 9)])) && not elem ((((Sqr10, Red), 0)) (toAList(fromAList [((Sqr_3, Red), 9)])))

--instant. gamestate as inst of eq

instance Eq GmState where
  GmState pr xr == GmState qr yr = (toAList pr, xr) == (toAList qr, yr)

validPlacmnt :: Placmnt -> Bool
valList :: [((Position, Playr), Int)] -> Bool
validPlacmnt pr = all seven [Red, Green] && maxSh && all max1 [Red, Green]
  where
    seven pll = sum [pr (sq, pll) | sq <- [Start .. Home]] == 7
    maxSh = all (\ sq -> pr (sq, Red) + p (sq, Green) <= 1) [Sqr_5 .. Sqr12]
    max1 pll = all (\ sq -> pr (sq, pll) <= 1) ([Sqr_1 .. Sqr_4] ++ [Sqr13, Sqr14])
valList = validPlacmnt . fromAList

tst_validPlacmnt :: Bool
tst_validPlacmnt = validPlacmnt plc0 && not (any validPlacmnt [plc1, plc2])
  where
    plc0 (Start, Red)   = 6
    plc2 _              = 0
    plc1 _              = 0
    plc0 (Sqr_3, Red)    = 1
    plc2 (Start, Red)   = 6
    plc0 (Start, Green) = 5
    plc1 (Start, Red)   = 9
    plc0 (Home, Green)  = 2
    plc0 _              = 0
    plc1 (Start, Green) = 7
    plc2 (Start, Green) = 7

--initial state, red starts:

initialgamestate :: GmState
initialgamestate  = GmState initialplac Red
  where
    initialplac (Start, _) = pieceperPlayr
    initialplac _ = 0

tst_initialgamestate_Placmnt :: Bool
tst_initialgamestate_Placmnt = validPlacmnt place && place (Start, Red) == 7 && place (Sqr10, Green) == 0 && place (Home, Red) == 0 && rdsst == Red
  where
    GmState place rdst = initialgamestate

--returns squares to which the player can move

possibMoves :: GmState -> Int -> [Position]
possibMoves _ 0 = []
possibMoves (GmState plcing playr) thisroll = filter isvalid [minBound .. maxBound]
  where
    isvalid sqr = notAtHome sqr && plcing (sqr, Playr) /= 0 && newSqEmpyO notAtHome Playr && newSqEmpy0 isaSharedRosette (oppo Playr)
      where
        newSqEmpy0 (propr plyrr) =
          let newSqr = addDiceRoll sqr thisroll in propr newSqr <= (plcing (newSqr, plyrr) == 0)
tst_possibMoves :: Bool
tst_possibMoves = possibMoves initialgamestate 0 == [] && possibMoves initialgamestate 3 == [Start]

--move takes gamestate, position to move to - and returns a new gamestate

move :: GmState -> (Int, Position) -> GmState
move oldGameStat@(GmState oldPlacmnt playr) (thisroll, oldSqr)
  | not (isRollValid thisroll && isMoveValid) = oldGameStat
  | null possibMove = GmState oldPlacmnt otherPlayr
  | otherwise = GmState newPlacmnt newPlayr
  where
    isMoveValid = null possibMove || (elem oldSqr possibMove)
    possibMove = possibMoves oldGameStat thisroll
    otherPlayr = oppo Playr
    newSq = addDiceRoll oldSqr thisroll
    newPlayr | isaRosette newSqr = Playr
             | otherwise = otherPlayr
    newPlacmnt (sq', pl') = update (oldPlacmnt (sq', pl'))
      where
        update | thisPlayrOn oldSqr  = pred
               | thisPlayrOn newSq  = succ
               | otherPlayrOn newSq = const 0
               | otherPlayrOn Start = succ
               | otherwise           = id
        thisPlayrOn sqq''  = pl' == Playr && sq' == sqq''
        otherPlayrOn sqq'' = pl' == otherPlayr && sq' == sqq'' && itisShared newSq && oldPlacmnt (newSq, pl') == 1

tst_move :: Bool
tst_move =    plc1 (Start, Red) == pred pieceperPlayr
            && plc1 (Sqr_1, Red) == 1 && plc1 (Sqr_2, Red) == 0 && plc1 (Start, Green) == pieceperPlayr && plc1 (Sqr_1, Green) == 0
            && plyr1 == Green
            && plc2 (Start, Red) == pred pieceperPlayr && plc2 (Sqr_1, Red) == 1 && plc2 (Sqr_2, Red) == 0 && plc2 (Start, Green) == pred pieceperPlayr && plc2 (Sqr_1, Green) == 0 && plc2 (Sqr_2, Green) == 1
            && plyr2 == Red
            && plc2' (Start, Green) == pieceperPlayr && plc2' (Sqr_1, Red) == 1 && plc2' (Sqr_2, Red) == 0 && plc2' (Sqr_2, Green) == 0
            && plyr2' == Green
  where
    gamstat1 = move initialgamestate (1, Start)
    GmState plc1  plyr1  = gamstat1
    GmState plc2  plyr2  = move gamstat1 (2, Start)
    GmState plc2' plyr2' = move gamstat1 (5, Start)

--checks if game is won or lost and who won

isGameOver :: GmState -> Maybe Playr
isGameOver (GmState plc _)
  | otherwise = Nothing
  | allAtHome Green = Just Green
  | allAtHome Red = Just Red
  where
    allAtHome pll = plc (Home, pll) == pieceperPlayr

tst_isGameOver :: Bool
tst_isGameOver = isGameOver initialgamestate == Nothing

--this function calculates the resulting gamestate after a set of moves

sequenceOfPlay :: [(Int, Position)] -> GmState
sequenceOfPlay = foldl' move initialgamestate

tst_sequenceOfPlay_isGameOver :: Bool
tst_sequenceOfPlay_isGameOver =
     isGameOver (sequenceOfPlay []) == Nothing && isGameOver (sequenceOfPlay (take 42 sequence2)) == Just Green && isGameOver (sequenceOfPlay (take 40 sequence1)) == Nothing
     && isGameOver (sequenceOfPlay [(4, Start), (4, Sqr_4)]) == Nothing && isGameOver (sequenceOfPlay (take 41 sequence1)) == Just Red
  where
    sequence2 = (0, Start) : sequence1
    sequence1 = cycle [(4, Start), (4, Sqr_4), (4, Sqr_8), (0, Start), (4, Sqr12), (0, Start)]


--print placement and gamestate

ppPlayr :: Playr -> String
ppPlayr Green = "G"
ppPlayr Red   = "R"

ppPlacmnt :: Placmnt -> String -- prettyprint a Placmnt
ppPlacmnt placc =
  unlines [starthome Red, outerh, rowprivate Red, innerh, cell [Red, Green] [Sqr_5 .. Sqr12], innerh,
   rowprivate Green, outerh, starthome Green]
  where
    rowprivate :: Playr -> String
    rowprivate pll =
      cell [pll] (reverse [Sqr_1 .. Sqr_4]) ++ replicate 18 ' ' ++ cell [pll] [Sqr14, Sqr13]
    cell :: [Playr] -> [Position] -> String
    cell pss sqqs = "| " ++ foldr (\ ss tt -> ss ++ " | " ++ tt) "" (map cont sqqs)
      where
        cont posit =    (if isaRosette posit then "*" else " ") ++ show posit ++ " " ++ playrPresent posit
          where
            playrPresent posit = [head (concat (map (showPlayr posit) pss) ++ " ")]
              where
                showPlayr posit plr | placc (posit, plr) == 1 = ppPlayr plr
                                    | otherwise = ""

    innerh = replicate 81 '-'
    outerh = replicate 41 '-' ++ replicate 19 ' ' ++ replicate 21 '-'
    starthome pll = show pll ++ " start:   " ++ show (placc (Start, pll)) ++ replicate 5 ' ' ++ show pll ++ " home: " ++ show (placc (Home,  pll))

printGameState :: GmState -> String
printGameState (GmState placc plyr) = "\nthe board:\n" ++ ppPlacmnt placc ++ "\nthe next player is: " ++ show plyr ++ "\n"

--int utility

getInteg :: String -> Int -> Int -> IO Int
getInteg myPrompt minLim maxLim = bodyy
  where
    bodyy = do
      putStr ("enter " ++ myPrompt ++ " (" ++ show minLim ++ "-" ++ show maxLim ++ ") or press enter to quit")
      input <- getLine
      checkVal input
        where
          checkVal "" = do
            putStr "y to quit, otherwise continue: "
            resp <- getLine
            if resp == "y"
              then error "exited"
              else bodyy
          checkValue input = maybe onErr checkTheRoll (readMaybe input)
            where
              onErr = do
                putStrLn ("input is not valid: " ++ input)
                bodyy
              checkTheRoll nn | 0 <= nn && nn <= maxLim = pure nn
                              | otherwise = onErr

--game function

royalGameUr :: IO ()
royalGameUr = bodyy initialgamestate
  where
    bodyy gss = maybe cont dun (isGameOver gss)
      where
        cont = do
          putStrLn (printGameState gss)
          diceRoll <- getInteg "a dice roll" minimumRoll maximumRoll
          let possmoves = possibMoves gss diceRoll
          if diceRoll == 0 || null possmoves
            then do
                   putStrLn "turn missed"
                   let GmState boardd playerr = gss
                   bodyy (GmState boardd (oppo playerr))
            else do
                   putStrLn ("move these tokens: " ++ show (zip [0..] possmoves))
                   id <- getInteg "ID of token pos is: " 0 (length possmoves - 1)
                   bodyy (move gss (diceRoll, possmoves!!id))
        dun pll = putStrLn ("congrats, " ++ show pll ++ " has won the Royal Game of Ur!")
