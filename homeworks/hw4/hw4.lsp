; Name: Ryan Peterman
; UID: 704269982

; Args:
;   n (int) - represents number of variables
;   delta (int list list) - represents the CNF defined over n variables
; Returns:
;   list of ints which are the variables that satisfy the CNF
;   where the sign on the ints represent TRUE or FALSE
(defun sat? (n delta) 
    (sat_helper n '() delta)
)

; generates assignments
; checks cnf once partial assignment is full
; returns NIL if no possible assignment otherwise returns assignment
(defun sat_helper (n partial cnf)
    (cond
        ; cnf completely simplified
        ((null cnf) partial)
        ; all variables assigned and valid assignment
        ((and (= n 0) (check_cnf cnf partial)) partial)
        ; invalid assignment
        ((= n 0) NIL)
        ; recursive case
        (T 
            ; remove useless eelements from cnf
            (let* ((pos_cnf (clean_cnf cnf n))
                   ; determine if unit conflict in new cnf
                   (pos_conflict (is_conflicting_unit pos_cnf))
                   ; recurse for pos_assign if not conflicting
                   (pos_assign (and (not pos_conflict) (sat_helper (- n 1) (append (list n) partial) cnf))))
                (cond
                    ; if positive assignment valid
                    ((and (not (null pos_assign)) (listp pos_assign)) pos_assign)
                    ; try negative assignment
                    (T 
                        (let* ((neg_cnf (clean_cnf cnf (- n)))
                               (neg_conflict (is_conflicting_unit neg_cnf))
                               (neg_assign (and (not neg_conflict) (sat_helper (- n 1) (append (list (- n)) partial) cnf))))
                            (cond
                                ; negative assignment valid
                                ((and (not (null neg_assign)) (listp neg_assign)) neg_assign) 
                                ; no valid assignment in neg or positive assign
                                (T NIL)
                            ) ; end cond
                        ); end let  
                    )
                ) ; end cond
            ) ; end let
        )
        
        ; this is the idea with the above construct but returning what
        ; we want in the condition is tedious
        ; if we bind locals with let then we compute before necessary
        ;   enless we do one let at a time
        ; if we dont use let we repeat computation
        ; ; assign next variable as true and see if it returns assignment list
        ; ((listp (sat_helper (- n 1) (cons 1 partial))) return it) 
        ; ; assign next variable as false and see if it returns assignment list
        ; ((listp (sat_helper (- n 1) (cons (- 1) partial))) return it)
        ; ; returns NIL if no assignment worked
        ; (T NIL)
    )
)

; given a cnf determines returns new cnf with units removed
; TODO: we need to support partial assignments that are not
; sorted from the beginning
; iterates over clauses and checks their length
; if their lenght is one then wlil assign if not already assigned
; if already assigned then checks if it matches unit clause
; cleans up cnf and removes things that can be removed
; returns the new cnf and assignments added in

; returns true if a conflicting unit clause appears in cnf
(defun is_conflicting_unit (cnf)
    (conflicting_unit_helper (find_units cnf)) 
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

; (clean_cnf '((1 2 3) (-1) (4)) 1) -> ((-1) (4)) - seems to work
; iterates over cnf checking for assigned variable and returns new cleaned cnf
(defun clean_cnf (cnf assign)
    ; remove_curr first clause if assign is in it
    (let* ((remove_curr (clause_contains (first cnf) assign)))
        (cond
            ; base case
            ((null cnf) NIL)
            ; remove first element by returning the cleaned rest of cnf
            (remove_curr (clean_cnf (rest cnf) assign))
            ; don't remove first element
            (T (cons (first cnf) (clean_cnf (rest cnf) assign)))
        ); end cond
    ); end let
)


;iterates over a list of numbers to check if clause is true
(defun check_clause (clause assign) 
    (cond
        ; end of list
        ((null clause) NIL)
        ; a literal
        ((numberp clause) 
            (let* ((lit_assign (nth (- (abs clause) 1) assign)))
                ; literal is true if its sign matches its assignment
                (> (* clause lit_assign) 0)
            )         
        )
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

; Some notes on the ideas behind my approach:
;   - CNF is a conjunction (AND) of clauses
;   - a clause is a disjunction of literals
;   - a literal is a variable or the negation of it
;
; Idea:
;   - use DFS
;   - try all possibilities for variable 1 
;   - i.e. X, -X partial assignments then XY, X-Y, -XY, -X-Y partial assignments
;   - two functions one for conjunctions and one for disjunctions
;   - if disjunction evaluated to false, then whole thing is false so stop

; utility function for easier loading
(defun reload ()
    (load "hw4.lsp")
)
