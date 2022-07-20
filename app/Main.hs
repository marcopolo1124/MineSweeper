module Main where

import System.Random

import Control.Monad.State
    ( MonadState(get), MonadIO(liftIO), StateT(runStateT) )
import Control.Monad.Except ( ExceptT, runExceptT )

import Lib (  emptyBoard
            , clickS
            , generateMineList
            , putMinesOnBoardS
            , flagS
            , checkWinningS
            , Board)
import Data.Char ( toLower )


main :: IO (Either String ((), Board))
main = runExceptT $ runStateT game (emptyBoard 14 18)

game :: StateT Board (ExceptT String IO) ()
game = do
  start
  gameLoop

lPutStrLn = liftIO . putStrLn
lgetLine = liftIO getLine

start :: StateT Board (ExceptT String IO) ()
start = do
  board <- get
  liftIO $ print board
  gen <- liftIO getStdGen
  lPutStrLn "Enter the coordinate"
  fstcoord <- lgetLine
  let 
    (x, y) = read fstcoord :: (Int, Int)
    mineList = generateMineList gen 40 (x, y) board
  putMinesOnBoardS mineList
  clickS x y

gameLoop :: StateT Board (ExceptT String IO) ()
gameLoop = do
  b <- get
  liftIO $ print b
  lPutStrLn "What action, Enter f for flag or s for select"
  action <- lgetLine
  lPutStrLn "Coordinate for action"
  c <- lgetLine
  let
    (x, y) = read c :: (Int, Int)
  case toLower <$> action of
    "f" -> 
      do 
        flagS x y
        checkWinningS
        gameLoop
    "s" ->
      do
        clickS x y
        checkWinningS
        gameLoop
    _ -> 
        gameLoop
