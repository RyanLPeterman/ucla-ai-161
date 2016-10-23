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
; NOTE: current assignment returns (1 -1 1) instead of (1 -2 3)
(defun sat_helper (n partial cnf)
    (cond
        ; all variables assigned and valid assignment
        ((and (= n 0) (check_cnf cnf partial)) partial)
        ; invalid assignment
        ((= n 0) NIL)
        ; recursive case
        (T 
            (let* ((pos_assign (sat_helper (- n 1) (cons 1 partial) cnf)))
                (cond
                    ; if positive assignment valid
                    ((listp pos_assign) pos_assign)
                    ; try negative assignment
                    (T 
                        (let* ((neg_assign (sat_helper (- n 1) (cons (- 1) partial) cnf)))
                            (cond
                                ; negative assignment valid
                                ((listp neg_assign) neg_assign) 
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
        ((listp cnf) (and (check_clause (first cnf) assign) (check_clause (rest cnf) assign)))
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
