module AStar where

import Board
import Index
import Data.List (elemIndex, minimumBy)
import Heuristics

data Move = L | R | U | D
    deriving (Eq)

moveSet = [L, R, U, D] :: [Move]

instance Show Move where
    show move =
        case move of
            L -> "L"
            R -> "R"
            U -> "U"
            D -> "D"

-- type NodeSet = Map.Map NodeState Bool
type NodeSet = [NodeState]

data SearchState = SearchState {
    startBoard :: Board, -- holds the start board
    goalBoard :: Board, -- holds the goal board
    heuristic :: (Board -> Board -> Maybe Int), -- holds the heuristic function
    solutionDepth :: Int, -- holds the depth of the solution
    nodesGenerated :: NodeSet, -- holds the nodes that have been generated previously
    nodeQueue :: [NodeState], -- holds unexpanded nodes
    numNodesGenerated :: Int, -- holds number of generated nodes
    solutionMoves :: [Move], -- holds the moves to the solution
    solutionVals :: [Int] -- holds the f(n) values to the solution
}

data NodeState = NodeState {
    currentBoard :: Board, -- holds the current board
    fval :: Int, -- holds the f(n) value of the node
    depth :: Int, -- holds the depth of the node
    moves :: [Move], -- holds the moves from the ancestor nodes
    fvals :: [Int] -- holds the f(n) value of the ancestor nodes
}

instance Show NodeState where
    show (NodeState current fval depth moves fvals)
        = "\nCurrent Board: " ++ show current ++
          "\nF(n) value: " ++ show fval ++
          "\nDepth: " ++ show depth ++
          "\nMoves (newest first): " ++ show moves ++
          "\nF(n) values (newest first): " ++ show fvals ++
          "\n----------------------------------------"

instance Eq NodeState where
    (==) (NodeState currentLeft _ _ _ _) (NodeState currentRight _ _ _ _) =
        currentLeft == currentRight
    (/=) x y =
        not ((==) x y)

compareNode :: NodeState -> NodeState -> Ordering
compareNode (NodeState _ leftFVal _ _ _) (NodeState _ rightFVal _ _ _)
    | leftFVal > rightFVal = GT
    | leftFVal < rightFVal = LT
    | otherwise = EQ

instance Ord NodeState where
    compare x y = compareNode x y

instance Show SearchState where
    show (SearchState startBoard goalBoard _ solutionDepth nodesGenerated nodeQueue numNodesGenerated solutionMoves solutionVals)
        = "\nStart Board: " ++ show startBoard ++
          "\nGoal Board: " ++ show goalBoard ++
          "\nDepth: " ++ show solutionDepth ++
        --   "\nNodes Generated: " ++ show nodesGenerated ++
        --   "\nNode queue: " ++ show nodeQueue ++
          "\n# Nodes Generated: " ++ show (fromIntegral (length nodesGenerated)) ++
          "\nSolution Moves (newest first): " ++ show solutionMoves ++
          "\nSolution f(n)s (newest first): " ++ show solutionVals ++
          "\n---------------------------------------------"

-- swaps two elements in a list
swapTwo :: Int -> Int -> [a] -> [a]
swapTwo first second xs = zipWith (\x y -> 
    if x == first then xs !! second
    else if x == second then xs !! first
    else y) [0..] xs

-- generates new coord based on move
moveCoord :: (Int, Int) -> Move -> (Int, Int)
moveCoord (x, y) move =
    case move of
        L -> (x-1, y)
        R -> (x+1, y)
        U -> (x, y-1)
        D -> (x, y+1)

-- generates all possible moves for coord
validMovesCoord :: (Int, Int) -> [Move]
validMovesCoord coord = [ move | move <- moveSet, validCoord $ moveCoord coord move ]

-- generates all possible moves for current board
allMoves :: Board -> Maybe [Move]
allMoves current =
    -- find the blank position
    case elemIndex 0 current of
        Just blankIndex -> (Just moves)
            where
                -- find the valid moves for the blank position
                moves = validMovesCoord $ fromIndex blankIndex
        Nothing -> Nothing

-- generates new board given current board and a move
moveBoard :: Board -> Move -> Maybe Board
moveBoard current move =
    -- find the blank position
    case elemIndex 0 current of
        Just blankIndex -> (Just newBoard)
            where
                -- find the index of the blank tile once you move it accordinly
                moveIndex = toIndex $ moveCoord (fromIndex blankIndex) move
                -- generate the new board by swapping the blank with the tile its moving to
                newBoard = swapTwo blankIndex moveIndex current
        Nothing -> Nothing

-- bad code but running out of time
unsafeUnmaybe :: Maybe a -> a
unsafeUnmaybe maybe =
    case maybe of
        Just a -> a
        Nothing -> error "unsafeUnmaybe ran into some trouble"

nextNodeState :: Board -- Goal board
              -> (Board -> Board -> Int) -- f(n)
              -> NodeState -- Expanding node
              -> Move -- Current move
              -> NodeState -- New node
nextNodeState goal fn (NodeState current fval depth moves fvals) move =
    -- return newly generate node
    NodeState current' fval' depth' moves' fvals'
    where
        -- get the new current board
        current' = unsafeUnmaybe $ moveBoard current move
        -- get the new f(n) value
        fval' = fn current' goal
        -- increase depth counter by 1
        depth' = depth + 1
        -- add move to move list
        moves' = moves ++ move:[]
        -- add f(n) value to f(n) value list
        fvals' = fvals ++ fval':[]


possibleNodes :: Board -- Goal board
              -> (Board -> Board -> Maybe Int) -- Heuristic
              -> NodeState -- Expanding node
              -> [NodeState] -- List of new nodes
possibleNodes goal heuristic (NodeState current fval depth moves fvals) =
    -- generate all node states given new f(n) and valid moves
    map (nextNodeState goal fn (NodeState current fval depth moves fvals)) validMoves
    where
        -- generating new level, increasing depth (gn)
        gn = depth + 1
        -- generate new f(n)
        fn = constructFn gn heuristic
        -- get all valid moves given current board
        validMoves = unsafeUnmaybe $ allMoves current

constructFn :: Int -> (Board -> Board -> Maybe Int) -> (Board -> Board -> Int)
constructFn gn hn =
    -- return a function that adds a given g(n) value with a heuristic function
    (\x y -> (+) gn (unsafeUnmaybe $ hn x y))

-- finds the node with the lowest f(n) value in a list of nodes
nextBestNode :: [NodeState] -> NodeState
nextBestNode = minimumBy compareNode

-- given a list of already generated nodes and a list of possible nodes, return 
-- only the nodes that haven't already been generated
generateNewNodes :: NodeSet -> [NodeState] -> [NodeState]
generateNewNodes _ [] = []
generateNewNodes nodeset nodes =
    [node | node <- nodes, not $ elem node nodeset]

-- check if a node is a goal node
checkGoalNode :: Board -> NodeState -> Bool
checkGoalNode goal (NodeState current _ _ _ _) =
    goal == current

-- add a list of nodes to a list of already generated nodes
recordNewNodes :: NodeSet -> [NodeState] -> NodeSet
recordNewNodes nodeset nodes =
    nodeset ++ nodes

-- test data
testStartBoard = [2, 8, 3, 1, 6, 4, 7, 0, 5]
testGoalBoard = [1, 2, 3, 0, 8, 4, 7, 6, 5]
testFn = constructFn 0 manhattanHeuristic
testFval = testFn testStartBoard testGoalBoard
testNode = NodeState testStartBoard testFval 0 [] $ testFval:[]
testStartState = SearchState testStartBoard testGoalBoard manhattanHeuristic (-1) [] [] 0 [] []

-- run a recurisve astar algorithm
runAStar :: SearchState -> SearchState
runAStar (SearchState startBoard goalBoard heuristic solutionDepth nodesGenerated nodeQueue numNodesGenerated solutionMoves solutionVals) =
    -- check if the the algorithm has started
    case null nodesGenerated of 
        -- if it hasn't, generate the first node and call algorithm again
        True -> 
            runAStar $ SearchState startBoard goalBoard heuristic solutionDepth nodesGenerated' nodeQueue' numNodesGenerated' solutionMoves solutionVals
            where
                fn = constructFn 0 heuristic
                fval = fn startBoard goalBoard
                fvals = fval:[]
                node = NodeState startBoard fval 0 [] fvals
                nodesGenerated' = nodesGenerated ++ node:[]
                nodeQueue' = node:[]
                numNodesGenerated' = numNodesGenerated' + 1
        -- if it has, find the next best move
        False ->
            -- check if the next best move is the goal state
            case checkGoalNode goalBoard $ NodeState current fval depth moves fvals of
                -- if it is, return the goal state
                True ->
                    SearchState startBoard goalBoard heuristic depth nodesGenerated nodeQueue' numNodesGenerated moves fvals
                -- if it isn't, generate new nodes and record the data, before recursing again
                False ->
                    runAStar newState
                    where
                        newNodes = generateNewNodes nodesGenerated $ possibleNodes goalBoard heuristic $ NodeState current fval depth moves fvals
                        nodesGenerated' = recordNewNodes nodesGenerated newNodes
                        nodeQueue'' = nodeQueue' ++ newNodes
                        numNodesGenerated' =  numNodesGenerated + length newNodes
                        newState = SearchState startBoard goalBoard heuristic solutionDepth nodesGenerated' nodeQueue'' numNodesGenerated' solutionMoves solutionVals
            where
                -- get next best node
                (NodeState current fval depth moves fvals) = nextBestNode nodeQueue
                -- remove best node from unexpanded node queue
                nodeQueue' = filter ((/=) $ NodeState current fval depth moves fvals) nodeQueue