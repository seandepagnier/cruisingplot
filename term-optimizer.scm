;; Copyright (c) 2007 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;; Simple optimizer for large arithmetic expressions.  Applies the
;; distributive law and factors out common subexpressions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This code is R5RS, plus SORT, PRINT and ->STRING from Chicken.

;; To compile in Chicken, "csc -s -O2 term-optimizer.scm" will give you
;; a shared library you can load to provide the following procedures:
;;
;;   (optimize-expr expr)
;;
;;     Returns an optimized version of the symbolic expression `expr',
;;   which should be a mathematical expression in terms of symbols,
;;   numeric literals, and the operators: +, -, *, /.
;;
;;     (optimize-expr '(+ (* a b) (* a c)))
;;     => (* a (+ b c))
;;
;;   (pp-expr/C expr [name [type]])
;;
;;     Writes the symbolic s-expression `expr' (an expression of the
;;   above form, optionally with LET's) to the current-output-port as a
;;   C expression.  If `name' is specified, writes it as a C function
;;   definition using the given name.  Types are specified as `type',
;;   which defaults to "double."

;; History:
;; Version 0.3  (2007/02/20) - constant folding, refactoring (* -1 x)
;; Version 0.2  (2007/02/20) - more optimizations
;; Version 0.1  (2007/02/19) - initial version

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require-extension (srfi 1))

;; Compute the cost of an expression, an sexp allowing LET and
;; arithmetic operators.  An example of scaling operators is given
;; (typically DIV costs more than MUL costs more than ADD).  Currently
;; assumes intermediate computations are free (not necessarily so if
;; not all values fit in registers), and registers and immediates are
;; equally fast (true for x86, RISC usually requires some overhead for
;; loading the values).
(define (expr-cost x)
  (if (pair? x)
    (if (eq? 'let (car x))
      (fold + (expr-cost (caddr x)) (map expr-cost (map cadr (cadr x))))
      (fold +
            ;; (* (case (car x) ((/) 4) ((*) 3) (else 1)))
            (- (length (cdr x)) 1)
            (map expr-cost (cdr x))))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; expression utilities

;; number < symbol < list
(define (sub-expr< a b)
  (cond
    ((number? a) (or (not (number? b)) (< a b)))
    ((number? b) #f)
    ((symbol? a) (or (not (symbol? b)) (string<? (symbol->string a) (symbol->string b))))
    ((symbol? b) #f)
    ((pair? a)
     (or (not (pair? b))
         (let lp ((a a) (b b))
           (cond
             ((null? a) (not (null? b)))
             ((null? b) #f)
             ((sub-expr< (car a) (car b)) #t)
             ((sub-expr< (car b) (car a)) #f)
             (else (lp (cdr a) (cdr b)))))))
    (else (not (pair? b)))))

;; just sorts the terms lexicographically
(define (normalize-sub-expr x)
  (if (pair? x)
    (cons (car x) (sort (map normalize-sub-expr (cdr x)) sub-expr<))
    x))

;; assumes already normalized
(define (unique-exprs ls)
  (map cdr (group-with-counts (sort ls sub-expr<))))

(define (delete1 x ls)
  (let lp ((ls2 ls) (rev '()))
    (cond ((null? ls2) ls)
          ((equal? x (car ls2)) (append-reverse rev (cdr ls2)))
          (else (lp (cdr ls2) (cons (car ls2) rev))))))

(define (cartesian-product lol)
  (if (null? lol)
    (list '())
    (let ((l (car lol))
          (rest (cartesian-product (cdr lol))))
      (append-map
       (lambda (x)
         (map (lambda (sub-prod) (cons x sub-prod)) rest))
       l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (all-op-combinations op ls)
  (if (not (pair? ls))
    '()
    (let lp1 ((x (car ls)) (ys (cdr ls)) (pairs '()))
      (if (null? ys)
        (reverse pairs)
        (let lp2 ((ls ys) (pairs pairs))
          (if (null? ls)
            (lp1 (car ys) (cdr ys) pairs)
            (lp2 (cdr ls) (cons (list op x (car ls)) pairs))))))))

(define (all-possible-sub-exprs x)
  (if (pair? x)
    (case (car x)
      ((+ *) (append (all-op-combinations (car x) (cdr x))
                     (append-map all-possible-sub-exprs (cdr x))))
      ((- /) (append-map all-possible-sub-exprs (cdr x)))
      (else (error "unknown operation" (car x))))
    '()))

(define (group-with-counts ls)
  (if (null? ls)
    '()
    (let lp ((x (car ls)) (ls (cdr ls)) (count 1) (res '()))
      (cond
        ((null? ls) (reverse (cons (cons count x) res)))
        ((equal? x (car ls)) (lp x (cdr ls) (+ count 1) res))
        (else (lp (car ls) (cdr ls) 1 (cons (cons count x) res)))))))

(define (sub-expr-stats x)
  (group-with-counts
   (sort
     (map normalize-sub-expr (all-possible-sub-exprs x))
     sub-expr<)))

;; attempt to extract all elements from ls1 in ls2, returns the
;; remaining elements in ls2, or #f if ls1 isn't a subset
(define (can-extract ls1 ls2)
  (let lp1 ((ls1 ls1) (ls2 ls2))
    (if (null? ls1)
      ls2
      (let lp2 ((ls ls2) (rev '()))
        (cond
          ((null? ls) #f)
          ((equal? (car ls1) (car ls))
           (lp1 (cdr ls1) (append (reverse rev) (cdr ls))))
          (else (lp2 (cdr ls) (cons (car ls) rev))))))))

(define (replace-var x name to)
  (let lp ((x x))
    (cond
     ((pair? x)
      (if (eq? 'let (car x))
        (let ((vars (map car (cadr x)))
              (vals (map lp (map cadr (cadr x)))))
          (let ((body (if (memq name vars) (cddr x) (map lp (cddr x)))))
            (cons 'let (cons (map list vars vals) body))))
        (map lp x)))
     ((eq? x name) to)
     (else x))))

(define (replace-sub-expressions x from to)
  (let ((op (car from)) (args (cdr from)))
    (let lp ((x x))
      (if (pair? x)
        (let ((x (map lp x)))
          (if (eq? (car x) op)
            (cond
              ((can-extract args (cdr x))
               => (lambda (rest)
                    (if (pair? rest)
                      (let ((tail (lp (cons op rest))))
                        (cons op (cons to (if (pair? tail)
                                            (cdr tail)
                                            (list tail)))))
                      to)))
              (else x))
            x))
        x))))

(define *temp-count* 0)

;; FIXME: greedy - not necessarily optimal
(define (eliminate-sub-expressions x)
  (let lp ((x x))
    (if (not (pair? x))
      x
      (let ((stats (sort (sub-expr-stats x) (lambda (a b) (> (car a) (car b))))))
        (if (<= (caar stats) 1)
          x
          (let ((tmp (string->symbol
                      (string-append "t" (number->string *temp-count*)))))
            (set! *temp-count* (+ *temp-count* 1))
            `(let ((,tmp ,(cdar stats)))
               ,(lp (replace-sub-expressions x (cdar stats) tmp)))))))))

(define (inverse-op x)
  (case x ((+) '-) ((-) '+) ((*) '/) ((/) '*) (else #f)))

(define (symbol-fold x)
  (cond
    ;; handle canceled terms
    ((and (pair? x) (memq (car x) '(/ -)))
     (if (memq (cadr x) (cddr x))
       (let ((others (delete1 (cadr x) (cddr x))))
         (let ((res (if (eq? (car x) '-) 0 1)))
           (if (null? others)
             res
             (constant-fold (cons (car x) (cons res) others)))))
       x))
    ;; maybe (* x 2) => (+ x x), (+ x y x z x) => (+ (* x 3) y z) etc...
    (else x)))

(define (constant-fold x)
  (if (pair? x)
    (case (car x)
      ((let)
       (cons 'let
             (cons (map (lambda (v) (list (car v) (constant-fold (cadr v)))) (cadr x))
                   (map constant-fold (cddr x)))))
      ((+ * / -)
       (symbol-fold
        (let ((args (map constant-fold (cdr (flatten-expr x)))))
          (receive (lits others) (partition number? args)
            (if (and (pair? lits) (pair? (cdr lits)))
              (let ((res (eval (if (and (memq (car x) '(/ -)) (not (number? (car args))))
                                 (cons (inverse-op (car x)) lits)
                                 (cons (car x) lits))
                               (scheme-report-environment 5))))
                (cond
                  ((null? others)
                   res)
                  ((and (memq (car x) '(/ -)) (not (number? (car args))))
                   (cons (car x) (cons (car others) (cons res (cdr others)))))
                  (else
                   (cons (car x) (cons res others)))))
              (cons (car x) args))))))
      (else
       (map constant-fold x)))
    x))

(define (flatten-expr x)
  (if (pair? x)
    (case (car x)
      ((let)
       (cons 'let (cons (map (lambda (v) (list (car v) (flatten-expr (cadr v)))) (cadr x))
                        (map flatten-expr (cddr x)))))
      ((+ *)
       (receive (same other)
           (partition (lambda (y) (and (pair? y) (eq? (car x) (car y))))
                      (map flatten-expr (cdr x)))
         (cons (car x) (append other (apply append (map cdr same))))))
      (else (map flatten-expr x)))
    x))

(define (refactor*-1 x)
  (if (pair? x)
    (case (car x)
      ((let) (cons 'let (cons (map (lambda (v) (list (car v) (refactor*-1 (cadr v))))
                                   (cadr x))
                              (map refactor*-1 (cddr x)))))
      ((+)
       (receive (neg pos)
           (partition (lambda (y) (and (pair? y) (eq? '* (car y)) (memv -1 y)))
                      (map refactor*-1 (cdr x)))
         (if (pair? neg)
           (let* ((neg (map (lambda (y) (delete1 -1 y)) neg))
                  (neg-sum (if (pair? (cdr neg)) (cons '+ neg) (car neg))))
             (if (null? pos)
               (list '* -1 neg-sum)
               (list '-
                    (if (pair? (cdr pos)) (cons '+ pos) (car pos))
                    neg-sum)))
           x)))
      (else (map refactor*-1 x)))
    x))

(define (used-more-than-once? sym x)
  (let ((count 0))
    (call-with-current-continuation
     (lambda (return)
       (let lp ((x x))
        (if (pair? x)
          (if (eq? 'let (car x))
            (begin
              (for-each lp (map cadr (cadr x)))
              (if (not (memq sym (map car (cadr x))))
                (for-each lp (cddr x))))
            (for-each lp x))
          (if (eq? x sym)
            (begin
              (set! count (+ count 1))
              (if (>= count 2)
                (return #t))))))
       #f))))

(define (remove-extra-temps x)
  (if (pair? x)
    (if (eq? 'let (car x))
      (let lp ((ls (cadr x)) (body (remove-extra-temps (caddr x))) (vars '()))
        (if (null? ls)
          (if (null? vars)
            body
            `(let ,(reverse vars) ,body))
          (let ((var (caar ls)) (val (remove-extra-temps (cadar ls))))
            (if (or (number? val) (not (used-more-than-once? var body)))
              (lp (cdr ls) (replace-var body var val) vars)
              (lp (cdr ls) body (cons (list var val) vars))))))
      (map remove-extra-temps x))
    x))

(define (renumber-temps x)
  (let ((i 0))
    (let lp ((x x))
      (if (pair? x)
        (if (eq? 'let (car x))
          (let* ((tmps (map (lambda (v)
                            (let ((tmp (string->symbol (string-append "t" (number->string i)))))
                              (set! i (+ i 1))
                              tmp))
                          (cadr x)))
                 (vals (map-in-order lp (map cadr (cadr x)))))
            (let lp ((ls (map car (cadr x)))
                     (tmps tmps)
                     (vals vals)
                     (body (map-in-order lp (cddr x)))
                     (vars '()))
              (if (null? ls)
                (cons 'let (cons (reverse vars) body))
                (lp (cdr ls)
                    (cdr tmps)
                    (cdr vals)
                    (replace-var body (car ls) (car tmps))
                    (cons (list (car tmps) (car vals)) vars)))))
          (map-in-order lp x))
        x))))

;; FIXME: doesn't explore all combinations, need to expand fully first
(define (all-distributions x)
  (cond
    ((not (pair? x))
     (list x))
    ((eq? 'let (car x))
     (error "being lazy about let factoring"))
    ((not (any pair? (cdr x))) ; no sub-exprs to distribute
     (list x))
    ((eq? '+ (car x))
     (let ((terms (cdr x)))
       ;; loop over all possible factors
       (let lp1 ((factors (unique-exprs (append-map cdr (cdr x))))
                 (res (list x)))
         (if (null? factors)
           res
           (let ((a (car factors)))
             (let lp2 ((ls terms) (used '()) (unused '()))
               (if (null? ls)
                 (if (or (null? used) (null? (cdr used)))
                   (lp1 (cdr factors) res)
                   ;; yay, we can apply the distributive law!
                   (let ((dist (list '* a (cons '+ used))))
                     (if (null? unused)
                       (lp1 (cdr factors) (cons dist res))
                       (lp1 (cdr factors) (cons (cons '+ (cons dist unused)) res)))))
                 (cond
                   ((and (pair? (car ls))
                         (eq? '* (caar ls))
                         (pair? (cdar ls))
                         (can-extract (list a) (cdar ls)))
                    => (lambda (rem)
                         (if (null? (cdr rem))
                           (lp2 (cdr ls) (cons (car rem) used) unused)
                           (lp2 (cdr ls) (cons (cons '* rem) used) unused))))
                   (else (lp2 (cdr ls) used (cons (car ls) unused)))))))))))
    (else ; other arbitrary operator
     (cartesian-product (map all-distributions x)))))

;; Searches all refactorings via the distributive law for the most
;; optimal formulation according to EXPR-COST.
(define (optimize-expr x)
  (define (opt1-expr x)
    (refactor*-1 (flatten-expr (constant-fold (eliminate-sub-expressions x)))))
  (set! *temp-count* 0)
  (refactor*-1
   (flatten-expr
    (renumber-temps
     (remove-extra-temps
      (let opt ((x x))
        (if (pair? x)
          (if (eq? 'let (car x))
            ;; FIXME: let handling is sub-optimal, but not used as input
            (cons (car x)
                  (cons (map (lambda (v) (list (car v) (opt (cdr v))))
                             (cadr x))
                        (map opt (cddr x))))
            (let ((ls (all-distributions x))
                  (best (opt1-expr x)))
              (let lp ((ls ls) (cost (expr-cost best)) (best best))
                (if (null? ls)
                  best
                  (let* ((expr2 (opt1-expr (car ls)))
                         (cost2 (expr-cost expr2)))
                    (if (< cost2 cost)
                      (lp (cdr ls) cost2 expr2)
                      (lp (cdr ls) cost best)))))))
          x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (call-with-ids x proc)
  (let ((res
         (let lp ((x x) (bound '(* + - /)) (local '()) (free '()))
           (cond
             ((pair? x)
              (if (eq? 'let (car x))
                (let ((tmp (lp (cons '+ (map cadr (cadr x))) bound local free)))
                  (lp (caddr x)
                      (lset-union eq? (map car (cadr x)) bound)
                      (lset-union eq? (map car (cadr x)) (car tmp))
                      (cdr tmp)))
                (let lp2 ((ls x) (local local) (free free))
                  (if (null? ls)
                    (cons local free)
                    (let ((tmp (lp (car ls) bound local free)))
                      (lp2 (cdr ls) (car tmp) (cdr tmp)))))))
             ((symbol? x)
              (if (and (not (memq x bound)) (not (memq x free)))
                (cons local (cons x free))
                (cons local free)))
             (else
              (cons local free))))))
    (proc (sort (car res) sub-expr<) (sort (cdr res) sub-expr<))))

;; FIXME: doesn't break long lines
(define (pp-expr/C x . o)
  (define (prec a) (case a ((*) 1) ((/) 2) ((+) 3) ((-) 4) (else 5)))
  (define (prec< a b) (< (prec a) (prec b)))
  (define (prec!= a b) (not (= (prec a) (prec b))))
  (let* ((name (and (pair? o) (car o)))
         (type (or (and (pair? o) (pair? (cdr o)) (cadr o)) 'double))
         (indent (if name "    " "")))
    (define (pp x line? prec)
      (if line? (display indent))
      (cond
        ((and (pair? x) (eq? 'let (car x)))
         (pp (last x) line? prec))
        ((pair? x)
         (if (prec!= prec (car x)) (display "("))
         (let* ((op (car x))
                (str (string-append " " (->string op) " ")))
           (pp (cadr x) #f op)
           (for-each
            (lambda (y) (display str) (pp y #f op))
            (cddr x)))
         (if (prec!= prec (car x)) (display ")")))
        (else
         (write x)))
      (if line? (display ";\n")))
    (call-with-ids x
        (lambda (local free)
          (if name
            (begin
              (display type) (display " ") (display name) (display " (")
              (display (string-intersperse
                        (map (lambda (x) (string-append (->string type) " " x))
                             (map ->string free))
                        ", "))
              (display ") {\n")))
          (if (pair? local)
            (begin
              (display indent)
              (display type) (display " ")
              (display (string-intersperse (map ->string local) ", "))
              (display ";\n")))
          ;; write out let's
          (let lp ((x x))
            (cond
              ((and (pair? x) (eq? 'let (car x)))
               (let ((vals (map lp (map cadr (cadr x)))))
                 (for-each
                  (lambda (name val)
                    (display indent) (display name) (display " = ")
                    (pp val #f '=) (display ";\n"))
                  (map car (cadr x))
                  vals)
                 (last (map lp (cddr x)))))
              ((pair? x)
               (map lp x))
              (else
               x)))
          ;; write return value
          (display indent) (display "return ")
          (pp x #f 'top) (display ";\n")
          (if name (display "}\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example:

;; (-D*D*F*F + 2*C*D*F*G - C*C*G*G + D*D*E*H - 2*B*D*G*H + A*G*G*H
;;           - 2*C*D*E*I + 2*B*D*F*I + 2*B*C*G*I - 2*A*F*G*I - B*B*I*I
;;           + A*E*I*I + C*C*E*J - 2*B*C*F*J + A*F*F*J + B*B*H*J - A*E*H*J)
;;    / (C*C*E - 2*B*C*F + A*F*F + B*B*H - A*E*H)

;; rewrote subtractions as * -1, need to consider that in optimizations
(define test-expr
  '(/ (+ (* -1 D D F F) (* 2 C D F G) (* -1 C C G G) (* D D E H) (* -2 B D G H)
         (* A G G H) (* -2 C D E I) (* 2 B D F I) (* 2 B C G I) (* -2 A F G I)
         (* -1 B B I I) (* A E I I) (* C C E J) (* -2 B C F J) (* A F F J)
         (* B B H J) (* -1 A E H J))
      (+ (* C C E) (* -2 B C F) (* A F F) (* B B H) (* -1 A E H))))

;; testing in scheme
(define (make-fun x)
  (call-with-ids x (lambda (local free) (eval `(lambda ,free ,x) (scheme-report-environment 5)))))

;; (define f1 (make-fun test-expr))
;; (define f2 (make-fun (optimize-expr test-expr)))

'(do ((i 0 (+ i 1)))
    ((= i 100))
  (let ((r (lambda () (exact->inexact (/ (random 100) (+ 1 (random 100)))))))
    (let ((a (r)) (b (r)) (c (r)) (d (r)) (e (r)) (f (r)) (g (r)) (h (r)) (i (r)) (j (r)))
      (let ((x (f1 a b c d e f g h i j))
            (y (f2 a b c d e f g h i j)))
        (if (> (abs (if (zero? y) x (- 1 (/ x y)))) 0.1)
          (print "FAIL on inputs " (list a b c d e f g h i j) ": expected " x " got " y))))))

;; if you want to spit it back out to C
;; (pp-expr/C (optimize-expr test-expr) 'FOO 'double)

