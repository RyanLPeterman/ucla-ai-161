; evaluates to true if TREE contains N
(defun TREE-CONTAINS (N TREE)
    (cond
        ; base case
        ((numberp TREE) (= N TREE))
        ; recursive case - true if current node or child node contains N
        (T (or (TREE-CONTAINS N (first TREE)) (= (second TREE) N) (TREE-CONTAINS N (third TREE))))
    )
)
; evaluates to the max value given a TREE
(defun TREE-MAX (TREE)
    (cond
        ; if at a number presumably we have reached the rightmost elem
        ((numberp TREE) TREE)
        ; otherwise recurse on the larger side of the ordered tree
        (T (TREE-MAX (third TREE)))
    )
)

; evaluates to a list representing in order traversal of TREE
(defun TREE-ORDER (TREE)
    (cond
        ((numberp TREE) (list TREE))
        (T (append (TREE-ORDER (first TREE)) (list (second TREE)) (TREE-ORDER (third TREE))))
    )
)

