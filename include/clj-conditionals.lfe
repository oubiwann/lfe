;; Usage: `(clj:condp pred expr . clauses)`
;;
;; Given a binary predicate, an expression and a set of clauses of the form:
;;
;;     test-expr result-expr
;;
;;     test-expr >> result-fn
;;
;; where `result-fn` is a unary function, if `(pred test-expr expr)` returns
;; anything other than `undefined` or `false`, the clause is a match.
;; If a binary clause matches, return `result-expr`.  If a ternary clause
;; matches, call `result-fn` with the result of the predicate and return the
;; result.  If no clause matches and a single default expression is given after
;; the clauses, return it. If no default expression is given and no clause
;; matches, return a tuple of the form:
;;
;;     #(error "No matching clause: {{expr}}")
(defmacro clj:condp
  (`(,pred ,expr . ,clauses)
   (fletrec ((falsey? (x)
               `(lists:member ,x '(undefined false)))
             (emit
               ([pred expr `(,a >> ,c . ,more)]
                `(let ((p (funcall ,pred ,a ,expr)))
                   (if ,(falsey? 'p)
                     ,(emit pred expr more)
                     (funcall ,c p))))
               ([pred expr `(,a ,b . ,more)]
                `(if ,(falsey? `(funcall ,pred ,a ,expr))
                   ,(emit pred expr more)
                   ,b))
               ([pred expr `(,a)] a)
               ([pred expr '()]
                `#(error
                   ,(lists:flatten
                     `("No matching clause: " . ,(lfe_io_pretty:term expr)))))))
     (emit pred expr clauses))))

;; If `test` evaluates to `false`, evaluate and return `then`, otherwise `else`,
;; if supplied, else `undefined`.
(defmacro clj:if-not
  (`(,test ,then . ()) `(clj:if-not ,test ,then 'undefined))
  (`(,test ,then . (,else))
   `(if (not ,test) ,then ,else)))

;; If `test` evaluates to `false`, evaluate `body` in an implicit `progn`,
;; otherwise if `test` evaluates to `true`, return `undefined`.
(defmacro clj:when-not
  (`(,test . ,body)
   `(if ,test 'undefined (progn ,@body))))

;; Same as `(not (== ...))`
(defmacro clj:not=
  (`(,x . ())       'false)
  (`(,x ,y . ,more) `(not (== ,x ,y ,@more))))
