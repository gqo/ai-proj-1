package main

import (
	"errors"
	"log"
	"math"
)

var boardWidth int32 = 3
var boardLimit = boardWidth - 1

var testStart = newBoard([9]int32{4, 3, 1, 7, 0, 2, 8, 5, 6})
var testGoal = newBoard([9]int32{1, 2, 3, 8, 0, 4, 7, 6, 5})

type point struct {
	x, y int32
}

func (p point) toIndex() int32 {
	return p.x + (p.y * boardWidth)
}

func fromIndex(i int32) point {
	y := i / boardWidth
	x := i % boardWidth
	p := point{
		x: x,
		y: y,
	}
	return p
}

func (p point) isValid() bool {
	if (p.x < 0 || p.x > boardLimit) || (p.y < 0 || p.y > boardLimit) {
		return false
	}
	return true
}

func pointDistance(a, b point) int32 {
	xDistance := int32(math.Abs(float64(a.x - b.x)))
	yDistance := int32(math.Abs(float64(a.y - b.y)))
	return xDistance + yDistance
}

type board struct {
	board [9]int32
}

func newBoard(args [9]int32) board {
	b := board{
		board: args,
	}
	return b
}

func (b board) elemPoint(elem int32) (point, error) {
	for i := range b.board {
		if b.board[i] == elem {
			p := fromIndex(int32(i))
			return p, nil
		}
	}
	return point{}, errors.New("elemIndex error: element not found")
}

func manhattanDistance(current, goal board, elem int32) (int32, error) {
	cPoint, err := current.elemPoint(elem)
	if err != nil {
		return -1, err
	}
	gPoint, err := goal.elemPoint(elem)
	if err != nil {
		return -1, err
	}
	return pointDistance(cPoint, gPoint), nil
}

func manhattanSum(current, goal board) (int32, error) {
	var sum int32
	sum = 0
	for i := range current.board {
		elem := int32(i)
		dist, err := manhattanDistance(current, goal, elem)
		if err != nil {
			return -1, err
		}
		sum += dist
	}
	return sum, nil
}

func pointLinConflict(startA, goalA, startB, goalB point) bool {
	// log.Println("StartA:", startA)
	// log.Println("GoalA:", goalA)
	// log.Println("StartB:", startB)
	// log.Println("GoalB:", goalB)
	if (startA.x == goalA.x) && (goalA.x == startB.x) && (startB.x == goalB.x) {
		if (startA.y < startB.y) && (goalA.y > goalB.y) {
			return true
		} else if (startA.y > startB.y) && (goalA.y < goalB.y) {
			return true
		}
	} else if (startA.y == goalA.y) && (goalA.y == startB.y) && (startB.y == goalB.y) {
		if (startA.x < startB.x) && (goalA.x > goalB.x) {
			return true
		} else if (startA.x > startB.x) && (goalA.x < goalB.x) {
			return true
		}
	}
	return false
}

func linConflict(current, goal board, a, b int32) (bool, error) {
	startA, err := current.elemPoint(a)
	if err != nil {
		return false, err
	}
	goalA, err := goal.elemPoint(a)
	if err != nil {
		return false, err
	}
	startB, err := current.elemPoint(b)
	if err != nil {
		return false, err
	}
	goalB, err := goal.elemPoint(b)
	if err != nil {
		return false, err
	}
	return pointLinConflict(startA, goalA, startB, goalB), nil
}

func linConflictSum(current, goal board) (int32, error) {
	var sum int32
	sum = 0
	for i := range current.board {
		for j := range current.board {
			a := int32(i)
			b := int32(j)
			// log.Println("(a,b):", a, b)
			conflict, err := linConflict(current, goal, a, b)
			if err != nil {
				return -1, err
			}
			if conflict {
				// log.Println("Found a conflict")
				sum++
			}
		}
	}
	return sum, nil
}

type move string

const (
	lMove move = "L"
	rMove move = "R"
	uMove move = "U"
	dMove move = "D"
)

func movePoint(p point, mv move) point {
	switch mv {
	case lMove:
		return point{
			x: p.x - 1,
			y: p.y,
		}
	case rMove:
		return point{
			x: p.x + 1,
			y: p.y,
		}
	case uMove:
		return point{
			x: p.x,
			y: p.y - 1,
		}
	case dMove:
		return point{
			x: p.x,
			y: p.y + 1,
		}
	default:
		return p
	}
}

func validPointMoves(p point) []move {
	var moves []move
	if moved := movePoint(p, lMove); moved.isValid() {
		moves = append(moves, lMove)
	}
	if moved := movePoint(p, rMove); moved.isValid() {
		moves = append(moves, rMove)
	}
	if moved := movePoint(p, uMove); moved.isValid() {
		moves = append(moves, uMove)
	}
	if moved := movePoint(p, dMove); moved.isValid() {
		moves = append(moves, dMove)
	}
	return moves
}

func (b board) validMoves() ([]move, error) {
	blankPoint, err := b.elemPoint(int32(0))
	if err != nil {
		return nil, err
	}
	return validPointMoves(blankPoint), nil
}

func (b board) moveBoard(mv move) (board, error) {
	nextBoard := b

	blankPoint, err := b.elemPoint(int32(0))
	if err != nil {
		return board{}, err
	}

	// find the point that the blank is swapping with
	swapPoint := movePoint(blankPoint, mv)
	// temporarily store the value at that point
	temp := nextBoard.board[swapPoint.toIndex()]
	// move the blank
	nextBoard.board[swapPoint.toIndex()] = int32(0)
	// set the previous blank position to the swapped value
	nextBoard.board[blankPoint.toIndex()] = temp

	return nextBoard, nil
}

type heuristic bool

const (
	manhattanOnly heuristic = true
	manhattanLin  heuristic = false
)

func fn(gn int32, hn heuristic, current, goal board) (int32, error) {
	switch hn {
	case manhattanOnly:
		manhattan, err := manhattanSum(current, goal)
		if err != nil {
			return 1337, err
		}
		return gn + manhattan, nil
	case manhattanLin:
		manhattan, err := manhattanSum(current, goal)
		if err != nil {
			return 1337, err
		}
		conflicts, err := linConflictSum(current, goal)
		if err != nil {
			return 1337, err
		}
		return gn + manhattan + conflicts, nil
	}
	return 1337, errors.New("non valid heuristic")
}

type nodeState struct {
	current board
	hn      heuristic
	fVal    int32
	depth   int32
	moves   []move
	fVals   []int32
}

func (n nodeState) nextNode(goal board, mv move) (nodeState, error) {
	nextBoard, err := n.current.moveBoard(mv)
	if err != nil {
		return nodeState{}, err
	}

	nextDepth := n.depth + 1

	nextFVal, err := fn(nextDepth, n.hn, nextBoard, goal)
	if err != nil {
		return nodeState{}, err
	}

	nextMoves := append(n.moves, mv)
	nextFVals := append(n.fVals, nextFVal)

	return nodeState{
		current: nextBoard,
		hn:      n.hn,
		fVal:    nextFVal,
		depth:   nextDepth,
		moves:   nextMoves,
		fVals:   nextFVals,
	}, nil
}

func (n nodeState) possibleNodes(goal board) ([]nodeState, error) {
	moves, err := n.current.validMoves()
	if err != nil {
		return nil, err
	}
	var nodes []nodeState
	for i := range moves {
		node, err := n.nextNode(goal, moves[i])
		if err != nil {
			return nil, err
		}
		nodes = append(nodes, node)
	}
	return nodes, nil
}

func eqNode(a, b nodeState) bool {
	if (a.current == b.current) && (a.hn == b.hn) && (a.fVal == b.fVal) && (a.depth == b.depth) {
		return true
	}
	return false
}

func minNode(a, b nodeState) nodeState {
	if a.fVal < b.fVal {
		return a
	}
	return b
}

type searchState struct {
	start           board
	goal            board
	hn              heuristic
	solnDepth       int32
	genNodes        map[board]bool
	unexpandedNodes []nodeState
	numGenerated    int32
	solnMoves       []move
	solnFVals       []int32
}

func (s *searchState) findNextNode() (nodeState, error) {
	log.Println("Finding next node...")
	log.Println("Currently unexpanded:", s.unexpandedNodes)
	bestNode := nodeState{
		fVal: 99999,
	}
	for i := range s.unexpandedNodes {
		log.Println("Checking this node:", s.unexpandedNodes[i])
		bestNode = minNode(bestNode, s.unexpandedNodes[i])
	}
	if bestNode.fVal == 99999 {
		return nodeState{}, errors.New("no move found")
	}
	return bestNode, nil
}

func (s *searchState) popNode(n nodeState) {
	log.Println("Popping node...")
	log.Println("Popping:", n)
	var index int
	found := false
done:
	for index = range s.unexpandedNodes {
		if eqNode(s.unexpandedNodes[index], n) {
			log.Println("Found n in queue @:", index)
			found = true
			break done
		}
	}
	if found {
		s.unexpandedNodes = append(s.unexpandedNodes[:index], s.unexpandedNodes[index+1:]...)
	}
}

func (s *searchState) generateNewNodes(nextNode nodeState) error {
	log.Println("Generating new nodes...")
	nodes, err := nextNode.possibleNodes(s.goal)
	if err != nil {
		return err
	}
	log.Println("Possible nodes:", nodes)
	for i := range nodes {
		if _, ok := s.genNodes[nodes[i].current]; !ok {
			log.Println("Ungenerated node found:", nodes[i])
			s.genNodes[nodes[i].current] = true
			s.unexpandedNodes = append(s.unexpandedNodes, nodes[i])
			s.numGenerated++
		}
	}
	return nil
}

func (s *searchState) checkGoalFound(nextNode nodeState) bool {
	log.Println("Checking goal found...")
	return nextNode.current == s.goal
}

func newSearch(start, goal board, hn heuristic) (searchState, error) {
	fVal, err := fn(0, hn, start, goal)
	if err != nil {
		return searchState{}, err
	}

	var fVals []int32
	var moves []move
	fVals = append(fVals, fVal)

	startNode := nodeState{
		current: start,
		hn:      hn,
		fVal:    fVal,
		depth:   0,
		moves:   moves,
		fVals:   fVals,
	}

	var unexpandedNodes []nodeState
	unexpandedNodes = append(unexpandedNodes, startNode)

	genNodes := make(map[board]bool)
	genNodes[startNode.current] = true

	return searchState{
		start:           start,
		goal:            goal,
		hn:              hn,
		solnDepth:       -1,
		genNodes:        genNodes,
		unexpandedNodes: unexpandedNodes,
		numGenerated:    1,
		solnMoves:       moves,
		solnFVals:       fVals,
	}, nil
}

func (s *searchState) Run() error {
	done := false
	for !done {
		nextNode, err := s.findNextNode()
		if err != nil {
			return err
		}
		s.popNode(nextNode)
		if s.checkGoalFound(nextNode) {
			done = true
			s.solnDepth = nextNode.depth
			s.solnMoves = nextNode.moves
			s.solnFVals = nextNode.fVals
		} else {
			s.generateNewNodes(nextNode)
		}
	}
	return nil
}

func main() {
	log.Println("Initial board:", testStart)
	conflicts, _ := linConflictSum(testStart, testGoal)
	log.Println("Lin conflicts:", conflicts)
	manhattan, _ := manhattanSum(testStart, testGoal)
	log.Println("Manhattan:", manhattan)
	newBoard, _ := testStart.moveBoard(uMove)
	log.Println("Moved up:", newBoard)

	astar, err := newSearch(testStart, testGoal, manhattanLin)
	if err != nil {
		log.Println(err)
	}
	log.Println("Initial state:", astar)
	log.Println("Starting search...")
	err = astar.Run()
	if err != nil {
		log.Println(err)
	}
	log.Println("Finished search.")
	log.Println("Final state:", astar)
}
