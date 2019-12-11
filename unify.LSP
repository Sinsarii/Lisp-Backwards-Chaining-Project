
;==============================================================================
; function unify
;
;   This funtion will accept two terms and unify them with two-way pattern
;   matching.  It will return a list of substitutions sigma such that the car
;   of each element of the list is the variable to replace, and the cdr of
;   that element is the substitution that should be made for that variable.
;   If the two terms are identical, the list nil is returned.  If the two terms
;   cannot be unified, then nil is returned.
;==============================================================================

(defun unify (t1 t2)
  (let ((subst (unify! t1 t2 '(nil))))
    (if (equal subst '(nil))
        subst
        (remove nil subst)
    )
  )
)

;==============================================================================
; function unify!
;
;   This function is the whole heart of the program.  It will accept two terms
;   to unify, as well as a running substitution list sigma.  It will check the
;   following cases in order:
;     1.  Are t1 and t2 nil?
;         Make no substitution.
;     2.  Is t1 a variable?
;         If t1 and t2 are not the same and t1 is not in t2, then make the
;         substitution of t2 for t1.
;     3.  Is t2 a variable?
;         If t1 and t2 are not the same and t2 is not in t1, then make the
;         substitution of t1 for t2.
;     4.  Are t1 and t2 atoms?
;         If t1 and t2 are equal, the make no new substituion, otherwise,
;         return nil.
;     5.  Is either t1 or t2 and atom?
;         Return nil.
;     6.  Otherwise, make a substituion with the car of t1 and the car of t2,
;         and add it to the list of substitutions formed when the cdr of t1
;         is unified! with the cdr of t2.
;==============================================================================

(defun unify! (t1 t2 subst)
  (cond ((and (null t2) (null t1)) subst)
        ((is-var t1) (if (equal t1 t2)
                         subst
                         (if (not (part-of t1 (list t2)))
                             (cons (cons t1 t2) subst)
                         )
                     )
        )
        ((is-var t2) (if (equal t1 t2)
                         subst
                         (if (not (part-of t2 (list t1)))
                             (cons (cons t2 t1) subst)
                         )
                     )
        )
        ((and (atom t1) (atom t2))
         (if (eq t1 t2) subst nil)
        )
        ((or (atom t1) (atom t2)) nil)
        (t (let ((sub (unify (car t1) (car t2))))
             (cond ((null sub) nil)
                   ((equal sub '(nil))
                    (unify! (cdr t1) (cdr t2) subst)
                   )
                   (t (unify! (applysub (append sub
                                                (applysub sub subst)
                                        )
                                        (cdr t1)
                              )
                              (applysub (append sub
                                                (applysub sub subst)
                                        )
                                        (cdr t2)
                              )
                              (append sub (applysub sub subst))
                      )
                   )
             )
           )
        )
  )
)

;==============================================================================
; function applysub
;
;   This function will accept a substitution list and a term, then apply all
;   the substituions to that term, returning the term.
;==============================================================================

(defun applysub (subst term)
  (if (equal subst '(nil)) term
      (do* ((subs subst (cdr subs)) (sub (car subs) (car subs)))
           ((null subs) term)
        (setq term (applyone sub term))
      )
  )
)

;==============================================================================
; function applyone
;
;   This function will accept a substitution and a term and apply that
;   substitution to the term, returning the term.
;==============================================================================

(defun applyone (sub term)
  (cond ((null term) nil)
        ((atom term) (if (eq term (car sub)) (cdr sub) term))
        (t (cons (applyone sub (car term)) (applyone sub (cdr term))))
  )
)

;==============================================================================
; function is-var
;
;   This function will accept an element and return a non-nil value if the
;   element is a u, v, w, x, y, or z.
;==============================================================================

(defun is-var (x)
  (member x '(u v w x y z x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15
              x16 x17 x18 x19 x20))
)

;==============================================================================
; function part-of
;
;   This function will accept an item to search and a list, and search that
;   list on all levels for the item.  If it is found, the item is returned,
;   otherwise nil is returned.
;==============================================================================

(defun part-of (key lst)
  (cond ((null lst) nil)
        ((atom (car lst))
         (if (equal key (car lst))
             key
             (part-of key (cdr lst))
         )
        )
        ((listp (car lst))
         (if (null (part-of key (car lst)))
             (part-of key (cdr lst))
             key
         )
        )
  )
)
