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

; evaluates to the sub-list of L starting at START and have length LEN
(defun SUB-LIST (L START LEN)
    (cond
        ((null L) NIL)
        ((= LEN 0) NIL)
        ; START is now 0, we can start building up sublist by recursing on LEN
        ((= START 0) (cons (first L) (SUB-LIST (rest L) 0 (- LEN 1))) )
        ; START not equal to 0 yet so we don't build up list with cons yet
        (T (SUB-LIST (rest L) (- START 1) LEN))
    )
)
; evaluates to a list of two lists L1 and L2 such that 
; - L is the result of appending L1 and L2
; - Length of L2 minus length of L1 is 0 or 1
(defun SPLIT-LIST (L)
    (cond 
        ; if the list is even length we can just divide
        ((evenp (length L)) 
            (let* ((half_len (/ (length L) 2)))
                (list (SUB-LIST L 0 half_len) (SUB-LIST L half_len half_len))
            )
        )
        ; else an odd length list needs uneven partition and special handling
        (T
            (let* ((odd_half_len (/ (- (length L) 1) 2)))
                (list (SUB-LIST L 0 odd_half_len) (SUB-LIST L odd_half_len (+ odd_half_len 1)))
            ) 
        )
    )
)
(print "Sublist Testcases")
(print (equal '(a b c)(SUB-LIST '(a b c d) 0 3)))
(print (equal '(d) (SUB-LIST '(a b c d) 3 1)))
(print (equal NIL  (SUB-LIST '(a b c d) 2 0)))

; evaluates to the height of a binary tree
(defun BTREE-HEIGHT (TREE)
    (cond
        ((atom TREE) 0)
        (T (let* 
            (
                (l_height (+ 1 (BTREE-HEIGHT (first TREE))))
                (r_height (+ 1 (BTREE-HEIGHT (second TREE))))   
            )
            (cond
                ((< l_height r_height) r_height)
                (T l_height)
            )
        ))
    )
)

(print "B-Tree Height Testcases")
(print (= 0 (BTREE-HEIGHT 1)))
(print (= 1 (BTREE-HEIGHT '(1 2))))
(print (= 2 (BTREE-HEIGHT '(1 (2 3)))))
(print (= 2 (BTREE-HEIGHT '((1 2) (3 4)))))
(print (= 3 (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7))))))
(print (= 3 (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8))))))


