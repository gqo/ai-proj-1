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
        if listEquals $ leftStartX:leftGoalX:rightStartX:rightGoalX:[]
        then 
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
        else if listEquals $ leftStartY:leftGoalY:rightStartY:rightGoalY:[]
        then
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

mBoolToInt :: Maybe Bool -> Maybe Int
mBoolToInt mbool =
    case mbool of
        Just val ->
            if val then (Just 1)
            else (Just 0)
        Nothing -> Nothing

mBoolToIntList :: [Maybe Bool] -> [Maybe Int]
mBoolToIntList = map mBoolToInt

sumMaybeBool :: [Maybe Bool] -> Maybe Int
sumMaybeBool = sumMaybeInt . mBoolToIntList

t1 = [ 4, 3, 1,
       7, 0, 2,
       8, 5, 6 ] :: [Int]

t2 = [ 1, 2, 3,
       8, 0, 4,
       7, 6, 5 ] :: [Int]

linConflictSumVal :: Board -> Board -> Int -> Maybe Int
linConflictSumVal current goal val =
    sumMaybeBool conflicts
    where
        conflicts = map (linConflict current goal val) [1..8]

linConflictSum :: Board -> Board -> Maybe Int
linConflictSum current goal =
    sumMaybeInt conflictSums
    where
        conflictSums = map (linConflictSumVal current goal) [1..8]

testLinConflictSum :: Board -> Board -> Int
testLinConflictSum current goal =
    case linConflictSum current goal of
        Just sum -> sum
        Nothing -> -1