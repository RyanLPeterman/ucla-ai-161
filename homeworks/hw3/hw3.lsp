;
; CS161 HW3: Sokoban
; Name: Ryan Peterman
; UID: 704269982
; *********************
;    READ THIS FIRST
; *********************
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments. This file also contains many helper functions. You may call
; any of them in your functions.
;
; Do not modify a-star.lsp.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions.  Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). Of course, other versions of Lisp may also crash
; if the problem is too hard, but the amount of memory available will be
; relatively more relaxed. Improving the quality of the heuristic will mitigate
; this problem, as it will allow A* to solve hard problems with fewer node
; expansions. In either case, this limitation should not significantly affect
; your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time.
;
(defun reload ()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star ()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all ()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun


; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end

; Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
(defun goal-test (s)
  (cond
    ; base case row does not contain a block
    ((null s) T)
    ; if block not found in current row or rest of rows, goal-state
    ((listp s) (and (= (count box (first s)) 0) (goal-test (rest s))))
  );end cond
);end defun

; Modify this function to return the list of
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
;
; If you want to use it, you will need to set 'result' to be
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
;
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.

; returns int content of element at r c
; if outside contents of problem returns NIL
(defun get-square (s r c)
    (cond 
        ; if we still have rows to traverse
        ((> r 0) (get-square (rest s) (- r 1) c))
        ; we have the row we want now TODO this -1 might be dangerous
        ((= r 0) (get-square (first s) (- 1) c))
        ; if we still have cols to traverse
        ((> c 0) (get-square (rest s) (- 1) (- c 1)))
        ; done traversing rows and cols, return the value
        ((= c 0) (first s))
    );end cond
);end defun

; returns new state that is obtained by setting (r, c) -> v
; does not modify input state
; assumes it will never receive out of bounds row or column
(defun set-square (s r c v)
    (cond
        ; if we still have rows to traverse
        ((> r 0) (cons (first s) (set-square (rest s) (- r 1) c v)))
        ; we have the row we want now -1 signify done traversing rows
        ((= r 0) (cons (set-square (first s) (- 1) c v) (rest s)))
        ; if we still have cols to traverse
        ((> c 0) (cons (first s) (set-square (rest s) (- 1) (- c 1) v)))
        ; done traversing rows and cols, return the value
        ((= c 0) (cons v (rest s)))
    )
);end defun


; returns state that results from moving in direction D
; returns NIL if the move is invalid
; checking must happen here
;   N -> 1
;   S -> 2
;   E -> 3
;   W -> 4
(defun try-move (s d)
    (let* ((pos (getKeeperPosition s 0))
           (row (second pos))
           (col (first pos))
           ; position where keeper will go
           (nextPos 
                (cond
                    ; assuming top left was 0, 0
                    ; north
                    ((= d 1) (list (- row 1) col))
                    ; south
                    ((= d 2) (list (+ row 1) col))
                    ; east
                    ((= d 3) (list row (+ col 1)))
                    ; west
                    ((= d 4) (list row (- col 1)))
                ); end cond
           )
           ; position of block if pushed
           (nextNextPos
                (cond
                    ; north
                    ((= d 1) (list (- row 2) col))
                    ; south
                    ((= d 2) (list (+ row 2) col))
                    ; east
                    ((= d 3) (list row (+ col 2)))
                    ; west
                    ((= d 4) (list row (- col 2)))
                ); end cond
           )
           
           ; values of the squares in calculated positions
           (val (get-square s row col))
           (nextVal (get-square s (first nextPos) (second nextPos)))
           (nextNextVal (get-square s (first nextNextPos) (second nextNextPos)))
           
           ; these bools help us from trying moves that are known to be invalid at this point
           (isNextPosValid (and (>= (first nextPos) 0) (< (first nextPos) (length s)) (>= (second nextPos) 0) (< (second nextPos) (length (first s))))) 
           (isNextNextPosValid (and (>= (first nextNextPos) 0) (< (first nextNextPos) (length s)) (>= (second nextNextPos) 0) (< (second nextNextPos) (length (first s)))))
        
           ; next state with keeper moved if found valid move
           (keeperMovedState 
                (cond
                    ; if keeper spot was just a keeper set it to blank
                    ((isKeeper val) (set-square s row col blank))
                    ; if keeper spot was a keeper star, set it to a star
                    (T (set-square s row col star))
                ); end cond
           )
        ); end binding list
        
        ; check what type of square is in the direction we are looking
        (cond
            ; if position we attempt to travel is out of bounds return NIL
            ((not isNextPosValid) NIL)
            ; if its a blank we set-square to keeper
            ((isBlank nextVal) (set-square keeperMovedState (first nextPos) (second nextPos) keeper))
            ; if its a wall we return NIL
            ((isWall nextVal) NIL)
            ; if its a goal we set-square to keeper+goal
            ((isStar nextVal) (set-square keeperMovedState (first nextPos) (second nextPos) keeperstar))
            ; if its a box we must check one past the square to see if box has place to go
            ((isBox nextVal) 
                (cond
                    ; if position where we want to put the box is invalid return NIL
                    ((not isNextNextPosValid) NIL)
                    ; if its blank we move both keeper + box
                    ((isBlank nextNextVal) (set-square (set-square keeperMovedState (first nextPos) (second nextPos) keeper) (first nextNextPos) (second nextNextPos) box))
                    ; if its a goal we move both accordingly
                    ((isStar nextNextVal) (set-square (set-square keeperMovedState (first nextPos) (second nextPos) keeper) (first nextNextPos) (second nextNextPos) boxStar))
                    ; anything else we cannot move since box constrains us
                    (T NIL)
                ); end cond
            )
            ((isBoxStar nextVal)
                (cond
                    ; if position where we want to put the box is invalid return NIL
                    ((not isNextNextPosValid) NIL)
                    ; if its blank we move both keeper + box
                    ((isBlank nextNextVal) (set-square (set-square keeperMovedState (first nextPos) (second nextPos) keeperstar) (first nextNextPos) (second nextNextPos) box))
                    ; if its a goal we move both accordingly
                    ((isStar nextNextVal) (set-square (set-square keeperMovedState (first nextPos) (second nextPos) keeperstar) (first nextNextPos) (second nextNextPos) boxStar))
                    ; anything else we cannot move since box constrains us
                    (T NIL)
                ); end cond
            )
        ); end cond
    ); end let*
);

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 1) (try-move s 2) (try-move s 3) (try-move s 4)))
	 )
    (cleanUpList result);end
   );end let
  );

; Modify this function to compute the trivial
; admissible heuristic.
;
(defun h0 (s) 0)

; Modify this function to compute the
; number of misplaced boxes in s.
;
(defun h1 (s)
    (cond
        ; base case row does not contain a block
        ((null s) 0)
        ; add up count of blocks found in all rows 
        ((listp s) (+ (count box (first s)) (h1 (rest s))))
    ); end cond
); end defun

; Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this
; function to compute an admissible heuristic value of s.
;
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the
; running time of a function call.

; returns absolute value of v (spec didn't say we could use builtin)
(defun my_abs (v)
    (cond 
        ; if negative then negate
        ((< v 0) (- v))
        (T v)
    ); end cond
)

; given two numbers, returns the smaller
(defun my_min (n1 n2)
    (cond
        ; if only one is nil return non nil
        ((and (null n1) (not (null n2))) n2)
        ((and (null n2) (not (null n1))) n1)
        ; if both nil
        ((or (null n1) (null n2)) NIL)
        ; if n1 < n2 return n1
        ((< n1 n2) n1)
        (T n2)
    );end cond
)

; given two numbers, returns the larger
(defun my_max (n1 n2)
    (cond
        ; if only one is nil return non nil
        ((and (null n1) (not (null n2))) n2)
        ((and (null n2) (not (null n1))) n1)
        ; if both nil
        ((or (null n1) (null n2)) NIL)
        ; if n1 < n2 return n2
        ((< n1 n2) n2)
        (T n1)
    );end cond
)

; returns manhattan distance between two points
(defun manhattan (p1 p2)
    (cond 
        ; if either point is NIL 
        ((or (null p1) (null p2)) NIL)
        ; calc manhattan
        (T (let* ((x_dist (my_abs (- (first p1) (first p2))))
               (y_dist (my_abs (- (second p1) (second p2))))); end binding list
              (+ x_dist y_dist)
            ); end let
        )
    ); end cond
)

; keeps track of indexes in s
(defun get_val_helper (r c s v)
    (cond
        ; if s is nil return nil
        ((null s) NIL)
        ; if s is a number and a box, return its coords
        ((and (numberp s) (= s v)) (list(list r c)))
        ; s is a list of lists (we are traversing rows
        ((and (listp s) (listp (first s))) (append (get_val_helper r c (first s) v) (get_val_helper (+ r 1) c (rest s) v)))
        ; s is a list of numbers (we are traversing cols
        ((and (listp s) (numberp (first s))) (append (get_val_helper r c (first s) v) (get_val_helper r (+ c 1) (rest s) v)))
        ; is is a number and not a box
        (T NIL)
    ); end cond
)

; returns coordinates of all boxes in state
(defun get_boxes (s)
    (get_val_helper 0 0 s box)
)

; returns coordinates of all goals in state
(defun get_goals (s)
    (get_val_helper 0 0 s star)
)

; given a point and a list of other points
; returns min_dist between point and all other points
(defun min_dist (p l)
    (cond
        ; return a huge distance = inf
        ((null l) NIL)
        (T (my_min (manhattan p (first l)) (min_dist p (rest l))))
    ) ; end cond
)

(defun max_dist (p l)
    (cond
        ; return a huge distance = inf
        ((null l) NIL)
        (T (my_max (manhattan p (first l)) (max_dist p (rest l))))
    ) ; end cond
)
; given two lists of points, gets the min_dist between first point in l1
; and all of l2 and repeats for all values in l1 and sums them
(defun min_dist_helper (l1 l2)
    (cond
        ((null l1) 0)
        ; no points to calc distance between
        ((null l2) 0)
        ; calcs min dist for current point and adds recursive sum to it
        (T (+ (min_dist (first l1) l2) (min_dist_helper (rest l1) l2)))
    );end cond
)

; all the other heuristic ideas costed more time to compute than
; the time saved from expanding less nodes, ended up going with h1
(defun h704269982 (s)
    (h1 s)  
)
  ;(let*(;(boxes (get_boxes s))
          ;(goals (get_goals s))
          ;(pos (getKeeperPosition s 0))
          
          ; SLOWER dist to nearest box
          ;(dist_to_box (max_dist pos boxes))
          
          ; SLOWER dist between boxes and goals
          ;(tot_min_dist (min_dist_helper boxes goals))
     ;   ); end binding list
        

        ;(cond
            ; in case there were no boxes
        ;    ((null dist_to_box) 0)
            ; return smallest dist to box
        ;    (T (+ dist_to_box tot_min_dist))
        ;); end cond
    ;); end let
;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.  Each problem can be visualized by calling
 | (printstate <problem>).  Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem: 1) # of
 | nodes expanded by A* using our next-states and h0 heuristic.  2) the depth of
 | the optimal solution.  These numbers are located at the comments of the
 | problems. For example, the first problem below was solved by 80 nodes
 | expansion of A* and its optimal solution depth is 7.
 |
 | Your implementation may not result in the same number of nodes expanded, but
 | it should probably give something in the same ballpark. As for the solution
 | depth, any admissible heuristic must make A* return an optimal solution. So,
 | the depths of the optimal solutions provided could be used for checking
 | whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible
 | to solve without a good heuristic!
 |
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
