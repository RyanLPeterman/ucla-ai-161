; returns t if L2 is a subsequence of L1
(defun subseq (L1 L2)
    (cond
        ((null L2) T)
        ((null L1) NIL)
        ((equal (first L1) (first L2)) (subseq (rest L1) (rest L2)))
        (T (subseq (rest L1) L2))
    )
)

; replaces all instances of X in L with Y
(defun replace (X Y L)
    (cond
        ((null L) NIL)
        ((equal X (first L)) (cons Y (replace X Y (rest L))))
        (T (cons (first L) (replace X Y (rest L))))
    )
)

; replace nth element with R
(defun replace-element (L N R)
    (cond 
        ((null L) NIL)
        ((equal N 0) (cons R (rest L)))
        (T (cons (first L) (replace-element (rest L) (- N 1) R)))
    )
)

; return a new L with every element L duplicated
(defun duplicate (L)
    (cond
        ((null L) NIL)
        (T (append (list (first L) (first L)) (duplicate (rest L))))
    )
)

; returns T if X appears an even number of times in L
(defun appears-even (X L)
    (cond
        ((null L) NIL)
        ((equal X (first L)) (not (appears-even X (rest L))))
        (T (appears-even X (rest L)))
    )
)

; returns T if its a palindrome
(defun palindrome (L)
    ((null L) T)
    ((null (rest L)) T)
    ((equal (first L) (first (last L))) (palindrome (sublist L 1 (- (length L) 2))))
    (T NIL)
)

; Ordered Tree:
; - n number
; - (L m R) where m is a number and L and R are ordered Trees
; - every # in L <= m and every # in R >= m
; i.e. '(1 2 3)
(defun post-order (T)
    (cond
        ((numberp T) (list T)) 
        (T (append (post-order (first T)) (post-order (third T)) (list (second T))
    )
)
