module Index where

import Data.List (elemIndex)
import Data.Tuple (swap)

import Board

toIndex :: (Int, Int) -> Int
toIndex (x,y) = x + (y * boardWidth)

fromIndex :: Int -> (Int, Int)
fromIndex i = swap $ divMod i boardWidth

-- validSingle checks if x or y val is on game board
validSingle :: Int -> Bool
validSingle x
    | x < 0 = False
    | x > boardLimit = False
    | otherwise = True
    where
        boardLimit = boardWidth - 1

-- validCoord checks if co-ord given is on game board
validCoord :: (Int, Int) -> Bool
validCoord (x,y)
    | not $ validSingle x = False
    | not $ validSingle y = False
    | otherwise = True

-- finds the starting index and the goal index for a value
findStartGoalIndexes :: Board -> Board -> Int -> Maybe (Int, Int)
findStartGoalIndexes current goal element =
    case elemIndex element current of
        Just currentIndex ->
            case elemIndex element goal of
                Just goalIndex -> (Just (currentIndex, goalIndex))
                Nothing -> Nothing
        Nothing -> Nothing

-- returns the sum of a list of Maybe Int
sumMaybeInt :: [Maybe Int] -> Maybe Int
sumMaybeInt = fmap sum . sequence