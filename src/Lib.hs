module Lib where

import System.Random
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import ListZipper
import System.Random.Shuffle
import qualified Data.Set as S
import Data.Bool

data Space = Mine | Empty deriving (Show, Eq)
data Clicked = T Int | F | Flag deriving (Eq)

instance Show Clicked where
    show (T x) = show x
    show F = "_"
    show Flag = "F"

newtype Board = Board {getBoard :: NestedZipper (Space, Clicked)}

instance Show Board where
    show (Board board) =
        let l2 = show . (<$>) snd . toList <$> toList board
        in unlines l2

emptyBoard :: Int -> Int -> Board
emptyBoard rowNum columnNum =
    let
        row = fromList $ replicate columnNum (Empty, F)
    in Board $ fromList $ replicate rowNum row

getSize :: Board -> Int
getSize (Board board) = lengthZ board * lengthZ (getFocus board)

getNumColumns :: Board -> Int
getNumColumns (Board board) = lengthZ (getFocus board)

getNumRows :: Board -> Int
getNumRows = lengthZ . getBoard

type NumberOfMines = Int
type Position = (Int, Int)

shift1 :: Int -> [Int] -> [Int]
shift1 num lst =
    let shiftfunc x = if x >= num then x + 1 else x
    in shiftfunc <$> lst

convertIntToPos :: (Int, Int) -> Int -> Position
convertIntToPos (rows, columns) x =
    let
        x_pos = x `mod` columns
        y_pos = x `mod` rows
    in (x_pos, y_pos)

genRand :: StdGen -> Int -> [Int]
genRand _ 1 = []
genRand g size = let (a, newG) = random g
    in (a `mod`size):genRand newG (size - 1)

-- Shuffle a list randomly
randomShuffle :: StdGen -> [a] -> [a]
randomShuffle g lst =
    let ilst = genRand g (length lst)
    in shuffle lst ilst

generateMineList :: StdGen -> NumberOfMines -> Position -> Board -> [Position]
generateMineList g n clickedPos (Board board) =
    let rows = getNumRows (Board board)
        columns = getNumColumns (Board board)
        xPositions = [0..(columns - 1)]
        yPositions = [0..(rows - 1)]
        allPositions = do
            x <- xPositions
            y <- yPositions
            return (x, y)
        s = clickedPos:surroundPos clickedPos (Board board)
        removedClick = filter (`notElem` s) allPositions
    in
        take n $ randomShuffle g removedClick

getAllPos :: Board -> [(Int, Int)]
getAllPos b = do
    let rows = getNumRows b
        columns = getNumColumns b
        xPositions = [0..(columns - 1)]
        yPositions = [0..(rows - 1)]
    x <- xPositions
    y <- yPositions
    return (x, y)


checkWinning :: Board -> [Position] -> Bool
checkWinning (Board board) minePos =
    let allFlagged = foldr (\pos acc -> let
            Just sc = board !?! pos
            in acc && (snd sc == Flag)) True minePos
        allPos = S.fromList $ getAllPos (Board board)
        allEmptySpace = allPos `S.difference` S.fromList minePos
        allClicked = foldr (\pos acc -> let
            Just sc = board !?! pos
            in acc && (case snd sc of T _ -> True; _ -> False)) True allEmptySpace

    in allFlagged || allClicked

getMineList :: Board -> [Position]
getMineList board =
    let allPositions = getAllPos board
    in foldr (\x acc ->
        let Just (s, c) = getBoard board !?! x
        in if s==Mine then x:acc else acc) [] allPositions

checkWinningS :: StateT Board (ExceptT String IO) ()
checkWinningS = do
    b <- get
    let mList = getMineList b
        win = checkWinning b mList
    if win
        then do
            liftIO $ putStrLn "You Win"
            throwError "You Win"
        else do
            return ()


putMinesOnBoard :: [Position] -> Board -> Board
putMinesOnBoard minePos (Board board) =
    let
        Just b = foldl (
                \acc (x,y) -> do
                    b <- acc
                    nestedReplaceAt x y (Mine, F) b
                ) (Just board) minePos
    in Board b

-- A wrapper for the putMinesOnBoard function.
putMinesOnBoardS :: [Position] -> StateT Board (ExceptT String IO) ()
putMinesOnBoardS m=
    do
        b <- get
        let newBoard = putMinesOnBoard m b
        put newBoard

mineBoard :: StdGen -> NumberOfMines -> Position -> Board -> Board
mineBoard g n p b = putMinesOnBoard (generateMineList g n p b) b

surroundPos :: Position -> Board -> [Position]
surroundPos (x,y) board =
    let maxX = getNumColumns board - 1
        maxY = getNumRows board - 1
    in
        filter (\(x,y) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)
            [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1), (x+1, y+1), (x-1,y-1), (x+1, y-1), (x-1, y+1)]

countMines :: Board -> Position -> Int
countMines (Board board) pos =
    let posChecked = surroundPos pos (Board board)
        isMine = sequenceA $ (!?!) board <$> posChecked
    in
        case isMine of
            Nothing -> 0
            Just lst ->
                let mineList = filter (== Mine) $ fst <$> lst
                in length mineList

click :: Int -> Int -> Board -> Maybe Board
click x y (Board board) =
    fst <$> runStateT (clickState x y (Board board)) S.empty

flag :: Int -> Int -> Board -> Maybe Board
flag x y (Board board) = do
    reFocus <- nestedToIndex board x y
    let (s,c) = nestedFocus reFocus
    Just $ Board $ nestedReplace (s, Flag) reFocus

flagS :: Int -> Int -> StateT Board (ExceptT String IO) ()
flagS x y =
    do
        b <- get
        let afterFlag = flag x y b
        case afterFlag of
            Nothing -> do
                liftIO $ putStrLn "Bad coordinates"
                throwError "Bad coordinates"
            Just board -> do
                put board

-- A wrapper of click into a stateful computation. Helps with manager the board state
clickS :: Int -> Int -> StateT Board (ExceptT String IO) ()
clickS x y =
    do
        b <- get
        let afterClick = click x y b
        case afterClick of
            Nothing -> do
                liftIO $ putStrLn "Bad coordinates"
                throwError "Bad coordinates"
            Just board -> do
                put board

-- A stateful compututation with a Set of visited positions so computations do not repeat
-- A click on a space with 0 surrounding mines will prompt every square around it to be clicked
clickState :: Int -> Int -> Board -> StateT (S.Set Position) Maybe Board
clickState x y (Board board) =
    do
        visitedSpaces <- get
        if (x,y) `S.member` visitedSpaces
            then return (Board board)
            else do
                refocus <- lift $ nestedToIndex board x y
                let (s,c) = nestedFocus refocus
                if s == Mine
                    then lift Nothing
                    else
                        let mineCount = countMines (Board board) (x,y)
                            newSpace = (s, T mineCount)
                            currentBoard = Board $ nestedReplace newSpace refocus
                        in
                            if mineCount > 0
                                then return currentBoard
                                else
                                    do
                                        put (S.insert (x,y) visitedSpaces)
                                        let surrounding = surroundPos (x,y) (Board board)
                                        foldr (\(a,b) acc -> do
                                            newBoard <- acc
                                            clickState a b newBoard) (return currentBoard) surrounding

-- plan: wrap each function in a stateful computation with the board as the state
{- game cycle:  player selects the first square they want to click -> 
                create a randomly generated mineField using mineBoard ->
                click the first square ->
                loop asking player to flag or select a square
-}

testMineBoard n = mineBoard (mkStdGen 99) n (1,1) (emptyBoard 5 5)
mineList n = generateMineList (mkStdGen 100) n (1,1) (emptyBoard 5 5)

oneMine = testMineBoard 1