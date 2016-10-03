; CS161 Homework 1
; Name: Ryan Peterman
; UID: 704269982

;1. TREE-CONTAINS: evaluates to true if TREE contains N
(defun TREE-CONTAINS (N TREE)
    (cond
        ; if nil then nil
        ((null TREE) NIL)
        ; if a number then true
        ((numberp TREE) (= N TREE))
        ; recursive case - true if current node or child node contains N
        (T (or (TREE-CONTAINS N (first TREE)) (= (second TREE) N) (TREE-CONTAINS N (third TREE))))
    )
)

(print "Tree-Contains Testcases")
(print (equal T (TREE-CONTAINS 3 '((1 2 3) 7 8))))
(print (equal NIL (TREE-CONTAINS 4 '((1 2 3) 7 8))))

;2. TREE-MAX: evaluates to the max value given a TREE
(defun TREE-MAX (TREE)
    (cond
        ; if empty TREE no max
        ((null TREE) NIL)
        ; if at a number presumably we have reached the rightmost elem
        ((numberp TREE) TREE)
        ; otherwise recurse on the larger side of the ordered tree
        (T (TREE-MAX (third TREE)))
    )
)

(print "Tree-Max Testcases")
(print (equal 8 (TREE-MAX '((1 2 3) 7 8))))

;3. TREE-ORDER: evaluates to a list representing in order traversal of TREE
(defun TREE-ORDER (TREE)
    (cond
        ; empty tree has no inorder traversal
        ((null TREE) NIL)
        ; if leaf, return a list of that leaf
        ((numberp TREE) (list TREE))
        ; if not leaf, append the recursive calls together
        (T (append (TREE-ORDER (first TREE)) (list (second TREE)) (TREE-ORDER (third TREE))))
    )
)

(print "Tree-Order Testcases")
(print (equal '(3) (TREE-ORDER 3)))
(print (equal '(1 2 3 7 8) (TREE-ORDER '((1 2 3) 7 8))))

;4. SUB-LIST: evaluates to the sub-list of L starting at START and have length LEN
(defun SUB-LIST (L START LEN)
    (cond
        ; Empty list has no sublist
        ((null L) NIL)
        ; a list of no length
        ((= LEN 0) '())
        ; START is now 0, we can start building up sublist by recursing on LEN
        ((= START 0) (cons (first L) (SUB-LIST (rest L) 0 (- LEN 1))) )
        ; START not equal to 0 yet so we don't build up list with cons yet
        (T (SUB-LIST (rest L) (- START 1) LEN))
    )
)

(print "Sub-List Testcases")
(print (equal '(a b c)(SUB-LIST '(a b c d) 0 3)))
(print (equal '(d) (SUB-LIST '(a b c d) 3 1)))
(print (equal NIL (SUB-LIST '(a b c d) 2 0)))

;5. SPLIT-LIST: evaluates to a list of two lists L1 and L2 such that 
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

;6. BTREE-HEIGHT: evaluates to the height of a binary tree
(defun BTREE-HEIGHT (TREE)
    (cond
        ; empty tree has no height
        ((null TREE) NIL)
        ; leaves have height 0
        ((atom TREE) 0)
        (T (let* 
            (
                (l_height (+ 1 (BTREE-HEIGHT (first TREE))))
                (r_height (+ 1 (BTREE-HEIGHT (second TREE))))   
            )
            (cond
                ; take the max of the heights of the right and left subtrees
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

;7. LIST2BTREE: evaluates to takes a list of LEAVES and returns a balanced binary tree
(defun LIST2BTREE (LEAVES)
    (cond
        ; an empty list of LEAVES has no corresponding TREE
        ((null LEAVES) NIL)
        ; leaf case
        ((= (length LEAVES) 1) (first LEAVES))
        ; if list larger than two elements, split list and call LIST2BTREE on each
        (T (let*
            (
                (split_leaves (SPLIT-LIST LEAVES))
            )
            (list (LIST2BTREE (first split_leaves)) (LIST2BTREE (second split_leaves)))
        ))
    )
)


(print "List2Btree Testcases")
(print (equal 1 (LIST2BTREE '(1))))
(print (equal '(1 2) (LIST2BTREE '(1 2))))
(print (equal '(1 (2 3)) (LIST2BTREE '(1 2 3))))
(print (equal '((1 2) (3 4)) (LIST2BTREE '(1 2 3 4))))
(print (equal '((1 (2 3)) ((4 5) (6 7))) (LIST2BTREE '(1 2 3 4 5 6 7))))
(print (equal '(((1 2) (3 4)) ((5 6) (7 8))) (LIST2BTREE '(1 2 3 4 5 6 7 8))))

;8. evaluates to a list representing a BTREE, the inverse of LIST2BTREE
(defun BTREE2LIST (TREE)
    (cond
        ; an empty TREE has no corresponding list
        ((null TREE) NIL)
        ; leaf case
        ((atom TREE) (list TREE))
        ; TREE must be a list of two elements
        (T (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
    )  
)

(print "Btree2List Testcases")
(print (equal '(1) (BTREE2LIST 1)))
(print (equal '(1 2) (BTREE2LIST '(1 2))))
(print (equal '(1 2 3) (BTREE2LIST '(1 (2 3)))))
(print (equal '(1 2 3 4) (BTREE2LIST '((1 2) (3 4)))))
(print (equal '(1 2 3 4 5 6 7) (BTREE2LIST '((1 (2 3)) ((4 5) (6 7))))))
(print (equal '(1 2 3 4 5 6 7 8) (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))))

;9. a generic comparison function for two expressions
(defun IS-SAME (E1 E2)
    (cond
        ; NILS are the same
        ((and (null E1) (null E2)) T)
        ; if either is NIL and not both the same then not same
        ((or (null E1) (null E2)) NIL)
        ; numbers are same if equal
        ((and (numberp E1) (numberp E2)) (= E1 E2))
        ; lists are the same if heads and tails are the same
        ((and (listp E1) (listp E2)) (and (IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))))
        ; anything else is not the same
        (T NIL)
    )
)

(print "IS-SAME Testcases")
(print (equal T (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8))))
(print (equal NIL (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8))))

