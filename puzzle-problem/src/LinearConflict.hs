module LinearConflict where

import Index
import Board

-- listEquals checks if all elements in a list are the same
listEquals :: [Int] -> Bool
listEquals xs = and $ map (== head xs) (tail xs)

-- coordLinConflict returns if two vals (with start ang goal coords known) are 
-- in linear conflict. This function could use improvement. The nested ifs are 
-- ugly.
coordLinConflict :: 
       (Int, Int) -- startLeftCoord
    -> (Int, Int) -- goalLeftCoord
    -> (Int, Int) -- startRightCoord
    -> (Int, Int) -- goalLeftCoord
    -> Bool -- if linear conflict
coordLinConflict 
    (leftStartX, leftStartY) 
    (leftGoalX, leftGoalY) 
    (rightStartX, rightStartY) 
    (rightGoalX, rightGoalY) =
        -- are the coords in the same column?
        if listEquals $ leftStartX:leftGoalX:rightStartX:rightGoalX:[]
        then 
            -- check whether they're in a linear conflict within the column
            if leftStartY < rightStartY
            then 
                if leftGoalY > rightGoalY
                then True
                else False
            else if leftStartY > rightStartY
            then 
                if leftGoalY < rightGoalY
                then True
                else False
            else False
        -- are the coords in the same row?
        else if listEquals $ leftStartY:leftGoalY:rightStartY:rightGoalY:[]
        then
            -- check if they're in a linear conflict within the row
            if leftStartX < rightStartX
            then
                if leftGoalX > rightGoalX
                then True
                else False
            else if leftStartX > rightStartX
            then
                if leftGoalX < rightGoalX
                then True
                else False
            else False
        else False

-- linConflict returns whether two squares on a start board are in linear conflict
-- with reference to the goal board
linConflict :: Board -> Board -> Int -> Int -> Maybe Bool
linConflict current goal left right =
    case findStartGoalIndexes current goal left of
        Just (startLeft, goalLeft) ->
            case findStartGoalIndexes current goal right of
                Just (startRight, goalRight) ->
                    (Just conflict)
                    where
                        conflict = coordLinConflict (fromIndex startLeft) (fromIndex goalLeft) (fromIndex startRight) (fromIndex goalRight)
                Nothing -> Nothing
        Nothing -> Nothing

-- conv maybe bool to maybe int
mBoolToInt :: Maybe Bool -> Maybe Int
mBoolToInt mbool =
    case mbool of
        Just val ->
            if val then (Just 1)
            else (Just 0)
        Nothing -> Nothing

mBoolToIntList :: [Maybe Bool] -> [Maybe Int]
mBoolToIntList = map mBoolToInt

-- returns the sum of a list of bools as an int
sumMaybeBool :: [Maybe Bool] -> Maybe Int
sumMaybeBool = sumMaybeInt . mBoolToIntList

-- for one value on the board, compute all its linear conflicts
linConflictSumVal :: Board -> Board -> Int -> Maybe Int
linConflictSumVal current goal val =
    sumMaybeBool conflicts
    where
        conflicts = map (linConflict current goal val) [1..8]

-- for all values on the board, compute the total linear conflicts
linConflictSum :: Board -> Board -> Maybe Int
linConflictSum current goal =
    sumMaybeInt conflictSums
    where
        conflictSums = map (linConflictSumVal current goal) [1..8]