; Name: Ryan Peterman
; UID: 704269982

; Args:
;   n (int) - represents number of variables
;   delta (int list list) - represents the CNF defined over n variables
; Returns:
;   list of ints which are the variables that satisfy the CNF
;   where the sign on the ints represent TRUE or FALSE
(defun sat? (n delta) 
    (dpll delta (range 1 n) '())
    ;(length (dpll delta (range 1 n) '()))
    ;(check_cnf delta (dpll delta (range 1 n) '()))
)

; returns a list of ints (beg -> end)
(defun range (beg end)
    (cond 
        ; beg > end we terminate
        ((> beg end) NIL)
        (T (cons beg (range (+ beg 1) end)))
    )
)

; given a list, removes all duplicates
(defun remove_dups (l)
    (cond
        ((null l) NIL)
        ; remove duplicates remove head from tail
        (T (cons (first l) (remove_elem (first l) (remove_dups (rest l)))))
    )
)

; given two sets s1 and s2,
; returns s1 - [abs(s) for s in s2]
(defun set_diff (s1 s2)
    (cond
        ; no more elements to remove from s1
        ((null s2) s1)
        ; removes abs of first value in s2 from s1 then recurses
        (T (set_diff (remove_elem (abs (first s2)) s1) (rest s2)))
    )
)

; given an element and a set
; returns set with element removed
(defun remove_elem (elem s)
    (cond
        ((null s) NIL)
        ; if element matches, return rest
        ((= elem (first s)) (rest s))
        ; not matching, just recurse
        (T (cons (first s) (remove_elem elem (rest s))))
    )
)

; given a list of literals and a cnf
; calls helper function and returns new cnf
(defun propagate_literals (literals cnf)
    (let* ((after_prop (unit_propagate (first literals) cnf)))
        (cond
            ((null literals) cnf)
            ((and (numberp after_prop) (= after_prop (- 1))) (- 1))
            ; pass cnf down and propagate on current first
            (T (propagate_literals (rest literals) after_prop))
        ); end cond
    ); end let
)

; given a literal and a cnf
; remove all clauses in cnf that contain the literal
; remove all neg literals of non unit clauses
(defun unit_propagate (literal cnf)
    ; conflicting literal assignment with units
    (let* ((units (remove_dups (find_units cnf)))
           (conflicting_unit (and (listp units) (numberp literal) (conflicting_unit_helper (cons literal units)))))
        (cond
            ; null literal return cnf without doing anything
            ((null literal) cnf)
            ; conflicting unit signal termination
            (conflicting_unit (- 1))
            ; base case
            ((null cnf) NIL)
            ; clause is removed
            ((clause_contains (first cnf) literal) (unit_propagate literal (rest cnf)))
            ; clause contains negation, remove variable from clause
            ((clause_contains (first cnf) (- literal)) (cons (remove_elem (- literal) (first cnf)) (unit_propagate literal (rest cnf))))
            ; clause not removed
            (T (cons (first cnf) (unit_propagate literal (rest cnf))))
        ); end cond
    ); end let*
)

(defun dpll (cnf remaining assign)
    ; if cnf not null, binds the following for checking before recursion
    (let* ((not_done (not (null cnf)))
           (units (and not_done (remove_dups (find_units cnf))))
           ; removes all clauses containing unit assignments
           (new_cnf (and not_done (propagate_literals units cnf)))
           ; new_remain uing with units taken away
           (new_remain (and not_done (set_diff remaining units)))
           ; adds all units to assignment
           (unit_assign (and not_done (append assign units))))
        (cond
            ; if cnf is full of empty clauses/is empty return assign + remaining
            ((null cnf) (append assign remaining))
            ; if generating new_cnf signaled early termination, terminate
            ((and (numberp new_cnf) (= new_cnf (- 1))) NIL)
            ; if cnf after unit propagation is done
            ((null new_cnf) (append unit_assign new_remain))
            
            ; otherwise we recurse
            (T 
                (let* (
                       
                       ; grab one of the remaining elems
                       (pos_assign (first (first new_cnf)))
                       ; remove all clauses containing next_assign
                       (new_units (remove_dups (find_units new_cnf)))
                       (pos_cnf (and (unit_propagate pos_assign new_cnf)))
                       (early_failure (and (numberp pos_cnf) (= pos_cnf (- 1))))
                       ; remove assignment from remaining for pos case
                       (pos_remaining (and (not early_failure) (remove_elem (abs pos_assign) new_remain)))
                       (pos_res (and (not early_failure) (dpll pos_cnf pos_remaining (cons pos_assign unit_assign)))))
                       
                       (cond
                            ; conflicting clause found terminate early
                            (early_failure NIL)
                            ; setting pos gave a consistent assignment
                            ((not (null pos_res)) pos_res)
                            (T
                                ; tries the negation of the pos_assignment
                                (let* ((neg_assign (- pos_assign))
                                       (neg_cnf (and (unit_propagate neg_assign new_cnf)))
                                       (early_neg_fail (and (numberp neg_cnf) (= neg_cnf (- 1))))
                                       ; new remaining set
                                       (neg_remaining (and (not early_neg_fail) (remove_elem (abs neg_assign) new_remain)))
                                       ; tries first element in cnf as negative assignment
                                       (neg_res (and (not early_neg_fail) (dpll neg_cnf neg_remaining (cons neg_assign unit_assign)))))
                                    (cond
                                        ; terminate, early failure found
                                        (early_neg_fail NIL)
                                        ; neg assignment found a consistent result
                                        ((not (null neg_res)) neg_res)
                                        ; unsat
                                        (T NIL)
                                    ); end cond
                                ); end let*
                            )
                        ); end cond
                    ); end let*
            )
        ); end cond
    ); end let*
)

; returns true if conflicting unit appears among list
; iterates over units and selects the first
(defun conflicting_unit_helper (units)
    (cond
        ; no conflicts in null list
        ((null units) NIL)
        ; true if conflict among units and first of units or conflict in rest of units
        (T (or (neg_appears units (first units)) (conflicting_unit_helper (rest units))))
    )
)

; returns true if -n appears in l
(defun neg_appears (l n)
    (cond
        ((null l) NIL)
        (T (or (= (first l) (- n)) (neg_appears (rest l) n)))
    ); end cond
)

; returns all unit clauses
(defun find_units (cnf)
    (cond
        ((null cnf) NIL)
        ; first clause of cnf is a unit clause, return the variable
        ((= (length (first cnf)) 1) (cons (first (first cnf)) (find_units (rest cnf))))
        ; first clause is not a unit clause
        (T (find_units (rest cnf)))
    )
)

; does not work on empty clause
; iterates over clause checking for assigned variable and returns new cleaned clause
(defun clause_contains (clause assign)
    (cond
        ; empty clause
        ((null clause) NIL)
        ; true if either the current one matches or rest contains
        (T (or (= (first clause) assign) (clause_contains (rest clause) assign)))
    ); end cond
)

;iterates over a list of numbers to check if clause is true
(defun check_clause (clause assign) 
    (cond
        ; end of list
        ((null clause) NIL)
        ; a literal
        ((numberp clause) (member clause assign))
        ; a clause is true if either the first literal is true or the rest is true
        ((listp clause) (or (check_clause (first clause) assign) (check_clause (rest clause) assign)))
    )
)

; iterates over list of lists to check if assignment is valid
(defun check_cnf (cnf assign)
    (cond
        ; end of clauses (T so that and logic will be true if all are)
        ((null cnf) T)
        ; cnf true first cnf is true and rest of cnf's are true
        ((listp cnf) (and (check_clause (first cnf) assign) (check_cnf (rest cnf) assign)))
    )
)

; utility function for easier loading
(defun reload ()
    (load "hw4.lsp")
)
