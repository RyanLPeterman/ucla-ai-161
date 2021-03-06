; Name: Ryan Peterman
; UID: 704269982
; Discussion 1A

; #1: performs left to right DFS on input TREE and returns node visit order list
(defun DFS (TREE)
    (cond
        ; empty tree
        ((null TREE) NIL)  
        ; base case we are at a leaf
        ((atom TREE) (list TREE))
        ; recursive case, there are length TREE children
        ((list TREE) (append (DFS (first TREE)) (DFS (rest TREE))))
    )
)

; #2: applies DFID_HELPER to the tree repeatedly with larger and larger search depths
(defun DFID (TREE MAX_DEPTH)
    (cond
        ; appends prev smaller dfid traversal to current depth
        ((> MAX_DEPTH 0) (append (DFID TREE (- MAX_DEPTH 1)) (DFID_HELPER TREE MAX_DEPTH)))
        (T NIL)
    )
)

; performs actual DFID for each max_depth
(defun DFID_HELPER (TREE MAX_DEPTH)
    (cond
        ; when we get to end of level rest of TREE is NIL
        ((null TREE) NIL)
        ; if current TREE is a leaf, return it
        ((atom TREE) (list TREE))
        ; if we can go no deeper return NIL
        ((= MAX_DEPTH 0) NIL)
        ; recursive case, return DFID on leftmost subtree appended
        ; to DFID on rest in current level
        ((list TREE) (append (DFID_HELPER (first TREE) (- MAX_DEPTH 1)) (DFID_HELPER (rest TREE) MAX_DEPTH)))

    )
)

; These functions implement a depth-first iterative-deepening solver for the
; missionary-cannibal problem. In this problem, three missionaries and three
; cannibals are trying to go from the east side of a river to the west side.
; They have a single boat that can carry two people at a time from one side of
; the river to the other. There must be at least one person in the boat to cross
; the river. There can never be more cannibals on one side of the river than
; missionaries. If there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list (MISSIONARIES
; CANNIBALS SIDE). SIDE represents which side the boat is currently on, and is T
; if it is on the east side and NIL if on the west side. MISSIONARIES and
; CANNIBALS represent the number of missionaries and cannibals on the same side
; as the boat. Thus, the initial state for this problem is (3 3 T) (three
; missionaries, three cannibals, and the boat are all on the east side of the
; river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function ID-DFS, which is called
; with the initial state to search from and the depth up to which depth-first
; search will be performed. It returns the complete path from the initial state
; to the goal state: this path is a list of intermediate problem states. The
; first element of the path is the initial state and the last element is the
; goal state. Each intermediate state is the state that results from applying
; the appropriate operator to the preceding state.

; To solve the original problem, one would call (ID-DFS '(3 3 T) 0). 

; Examples of calls to some of the helper functions can be found after the code.


; FINAL-STATE takes a single argument (S), the current state, and returns T if
; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
    ; returns True if S is a list with the first and second elem = 3
    ; and the third element being NIL
    (equal s '(3 3 NIL))
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), a number of
; missionaries to move (M), and a number of cannibals to move (C). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
    (let* ((new_m (+ (- 3 (first s)) m))
           (new_c (+ (- 3 (second s)) c)))
        (cond
            ; if somehow > 3 missionaries or carnivores result
            ; or if number of carnivores on either side greater than
            ; missionaries and missionaries > 0
            ((or (> new_m 3) (> new_c 3) (and (> new_m 0) (> new_c new_m)) (and (> (- 3 new_m) 0) (> (- 3 new_c) (- 3 new_m)))) NIL)  
            ; otherwise return a list containing new state by flipping boat and setting
            ; number of missionaries and carnivores
            (T (list (list new_m new_c (not (third s)))))
        )
    )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (S), which encodes the current state, and
; returns a list of states that can be reached by applying legal operators to
; the current state.
(defun succ-fn (s)
    ; returns a list of all possible legal states
    (append
        ; tries all possible combinations of transporting 1-2 people
        ; Note: branching factor of 5
        (next-state s 0 1)
        (next-state s 1 0)
        (next-state s 0 2)
        (next-state s 2 0)
        (next-state s 1 1)
    )
)

; MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
; path from the initial state to the current state (PATH), the legal successor
; states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a single depth-first iteration to the given depth on
; each element of STATES in turn. If any of those searches reaches the final
; state, MULT-DFS returns the complete path from the initial state to the goal
; state. Otherwise, it returns NIL.

; path - path from init to current state
; states - list of states from current state
; depth - depth
(defun mult-dfs (states path depth) 
    (let* ((first_dfs (single-dfs (first states) path depth)))
        (cond
            ; if first-dfs returned something not null we have soln
            ((not (null first_dfs)) first_dfs)
            ; if dfs on first succ state is null try next
            ((not (null (rest states))) (mult-dfs (rest states) path depth))
            ; if first dfs not null just return the complete path
            (T NIL)
        )
    )
)

; SINGLE-DFS does a single depth-first iteration to the given depth. It takes
; three arguments: a state (S), the path from the initial state to S (PATH), and
; the depth (DEPTH). If S is the initial state in our search, PATH should be
; NIL. It performs a depth-first search starting at the given state. It returns
; the path from the initial state to the goal state, if any, or NIL otherwise.

; s - a state
; path - initial state to s
; depth - depth
(defun single-dfs (s path depth)
    (let* ((new_path (append path (list s))))
        (cond
            ; if current state is goal, then return path so far
            ((final-state s) new_path)
            ; not goal state and depth ran out
            ((< depth 0) NIL)
            ; not goal state, try single-dfs on all successor states
            ; with depth reduced by 1
            (T (mult-dfs (succ-fn s) new_path (- depth 1)))
        )
    )
)

; ID-DFS is the top-level function. It takes two arguments: an initial state (S)
; and a search depth (DEPTH). ID-DFS performs a series of depth-first
; iterations, starting from the given depth until a solution is found. It
; returns the path from the initial state to the goal state. The very first call
; to ID-DFS should use depth = 0.

; s - initial state
; depth - search depth
(defun id-dfs (s depth)
    (let* ((dfs_res (single-dfs s NIL depth)))
        (cond
            ((null dfs_res) (id-dfs s (+ depth 1)))
            (T dfs_res)
        )
    )
)

; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (note that NEXT-STATE
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; SUCC-FN returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))

