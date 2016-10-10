; performs left to right DFS on input TREE and returns node visit order list
(defun DFS (TREE)
    (cond
        ((null TREE) NIL)  
        ; base case we are at a leaf
        ((atom TREE) (list TREE))
        ; recursive case, there are length TREE children
        ((list TREE) (append (DFS (first TREE)) (DFS (rest TREE))))
    )
)

(print (DFS '((A (B)) C (D))))

; applies DFID_HELPER to the tree repeatedly with larger and larger search depths
(defun DFID (TREE MAX_DEPTH)
    (cond
        ((> MAX_DEPTH 0) (append (DFID TREE (- MAX_DEPTH 1)) (DFID_HELPER TREE MAX_DEPTH)))
        (T NIL)
    )
)

; performs actual DFID for each max_depthb
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

(print (DFID '((A (B)) C (D)) 3))
(print (DFID '(A (B C) (D) (E (F G))) 3) )
