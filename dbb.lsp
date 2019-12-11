;dbb.lsp
;Nikolas Mikolaski
(load #p "c:/lisp/bwc/db.lsp")
(load #p "c:/lisp/bwc/unify.lsp")

;===============================================================================
;detachpairs
;the purpose of the function is to take in a list of dotted pairs and return the
;right element of each pair
;
;example I/O
;((y.pavlov)(y.john)) => (pavlov john)
;===============================================================================

(defun detachpairs (old newlist)

  (dolist (pair old (cdr newlist))
          (if (listp pair) (setq newlist (append newlist (cdr (list (car pair) (cdr pair)))))))
  )

;===============================================================================
;cleanup
;The purpose of this function is to filter the 3 possible outcomes that come from test!
;1. test has returned a YES
;     will return the atom YES
;2. test has returned a NO
;     will return the atom YES
;3. test has returned a list of matches
;     will call the function detachpairs on the list of pairs and return a new list of atoms
;
;example I/O
;(YES) => YES
;(NO) => NO
;(NIL (y.pavlov) (y.john) => (pavlov john)
;===============================================================================
(defun cleanup (results)

  (cond ((equal (car results) 'YES) 'YES)
        ((AND (equal (car results) 'NO) (OR (equal (cdr results) 'NIL) (equal (cdr results) '(NIL)))) 'NO)
        (T (remove 'nil (detachpairs results '(nil))))

    )

  )
;===============================================================================
; ?
;This function acts as a driver for the main function ??
;it's purpose is to allow the user to enter query without having to input the
;database or the default sigma. In addition, ? sets the default sigma to NO (false)
;so that it will always return false unless conditions are met to set it to YES (true)
;or a list of matches.
;Example I/O
;('mortal x) => (socrates plato fido lassie felix leo)
;('dog fido) => YES
;('dog plato) => NO
;(x fido) => (dog mammal wb mortal)
;===============================================================================
(defun ? (query) (cleanup (?? query db '(NO))))

;===============================================================================
;??
;This function is the main function for the deductive database (dbb.lsp).
;It's helper functions are: unify and its helper functions
;                           cleanup
;It's purpose is to take in:
;       a query (the goal)
;       the database (db)
;       a list of matches (sigma) which is set to NO as default
;and search the entire database for elements that match the query.
;The database consists of a list of rules that are formatted as such:
; ((consequent)(antecedent))
;Examples: (T (dog fido) - fido is a dog
;          ((man x) (mortal x)) - if x is a man, then x is mortal
;As this deductive system uses backwards chaining, we must check the entire database's
;antecedent's to see if there is a match (the consequent is true).
; If the antecedent can be unified but it is not a match, we call ?? on the subtitution
; of the conesquent with the match made from the antecedent
; Example: (mortal plato) => ((man x) (mortal x)) (x.plato) => (man plato) =>
;?? acheives this by:
;     1. using a do-list to read every rule of the database, holding the sigma in parallel
;       to all depth first searched triggered by the do-list
;       do-list conditions:
;           A.If the goal matches exactly with the antecedent, set the sigma to YES
;           B.If the antecedent can be unified with the goal
;             AND the consequent is true
;                 add the resulting dotted pair from the two-way matched antecedent and goal to the sigma
;           C.If the unfication of the of the antecedent is possible BUT the following
;             subtitution on the antecedent will lead to a YES, no a pairing
;               1. Return YES if the pairing from the antecedent using the query variable does not carry to the consequent
;                     This means it is true for all
;               2. Return the subitution if the pairing does carry
;               Examples: (hates fido y) => ((likes Pavlov x11) (hates x11 x12)) ((x11.fido)(x12.y))=> YES
;                        (likes y fido) => ((dog x5) (likes Pavlov x5)) ((y.pavlov)(x5.dog)) => (pavlov)
;           D.If a unification is possible, and the substitution can be applied to the consequent, proceed farther in the tree
;           E.Otherwise, do nothing
;Example I/O: (dog fido) => (YES)
;             (x plato) => (NO (x.wb)(x.mortal)(x.man)(x.mammal))
;===============================================================================
(defun ?? (goal db sigma)
          (dolist (rule db sigma)

          (cond ((equal goal (cadr rule)) (setq sigma '(YES)))
                ((AND (unify goal (cadr rule))
                      (equal (car rule) T))
                        (setq sigma (append sigma (unify (cadr rule) goal )))
                )

                ((AND (unify goal (cadr rule))
                      (equal (?? (applysub (unify (cadr rule) goal) (car rule)) db sigma) '(YES))
                      (unify (applysub (unify (cadr rule) goal) (cadr rule)) goal )
                  )
                        (cond ((member 'NIL (unify (applysub (unify (cadr rule) goal) (cadr rule)) goal )) (setq sigma '(YES)))
                              (T (setq sigma (append sigma (unify (applysub (unify (cadr rule) goal) (cadr rule)) goal ))))))
                ((unify goal (cadr rule)) (setq sigma (?? (applysub (unify (cadr rule) goal) (car rule)) db sigma)))
          ))
     )
;===============================================================================
;add function to parse query and find if any atom is contained within the word bank
;fucntion thats sifts through database and looks at all consequents and adds them to word bank, ignore all variables
;function that rearranges query to fit database style
;test
;all macro-parameters
;===============================================================================
;Mulitple input



;===============================================================================


(defun ?M (listquerys)
    (cond   ((member (car listquerys) listquerys) (? (car listquerys))
            ;check if any query the same search term

            ;else, do two parallel searches
            (T (? query1) (?query2) )


      )
)
