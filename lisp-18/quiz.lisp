;; Mighty quiz































`(+ 2 ,(+ 1 5))


(eval `(+ 2 ,(+ 1 5)))


`(+ ,(+ 1 5) ,(+  2 (+ 3 4)))


`(+ ,(+ 1 5) `(+  2 ,(+ 3 4)))          ; 3 alernatives, see below:

;; 1:
(+ 6 `(+  2 7))

;; 2:
(+ 6 9)

;; 3
(+ 6 `(+ 2 ,(+ 3 4)))


`(+ ,(+ 1 5) ,`(+  2 ,(+ 3 4)))


(or (print "hello") (print " world"))


(loop for x in (list 1 4) collect (+ 2 x))


`(+ ,(loop for x in (list 3 4) collect (+ 2 x)))


`(+ ,@(loop for x in (list 3 4) collect (+ 2 x)))


`(+ ,@(loop for x in (list 3 4) collect `(+ 2 ,x)))
