# Schemy listy lispy lisps

+++ 
or

# Advanced Lisp for programming noobs


+++ 

Lisps:

* Common lisp

* Scheme

* Clojure

+++


## Prefix notation

* fac(5) -> (fac 5)

* gcd(6,3) -> (gcd 6 3)

* 4 + 2 -> (+ 4 2)
 
* 4 + 2 + 6 -> (+ 4 2 6) 
  
* 4 + (6 / 2) -> (+ 4 (/ 6 2))

## REPL examples

* (+ (print 3) (print 4))
  
* (if 't (print 3) (print 4))

* (if nil (print 3) (print 4))
  
* (list 2 (+ 1 5) "a string")

* '(list 2 (+ 1 5) "a string") 

* (eval '(list 2 (+ 1 5) "a string") )

* `(list 2 ,(+ 1 5) "a string")

* (progn (print "hello") (print "world")) # example




+++

## QUIZ!

* `(+ 2 ,(+ 1 5) )

* (eval `(+ 2 ,(+ 1 5) ) )

* `(+ ,(+ 1 5) ,(+  2 (+ 3 4)))

* `(+ ,(+ 1 5) `(+  2 ,(+ 3 4)))

* `(+ ,(+ 1 5) ,`(+  2 ,(+ 3 4)))

* (or (print "hello") (print " world"))

* (loop for x in (list 1 4 6 3) collect (+ 2 x))

* `(+ ,(loop for x in (list 3 4) collect (+ 2 x)))

* `(+ ,@(loop for x in (list 3 4) collect (+ 2 x)))

* `(+ ,@(loop for x in (list 3 4) collect `(+ 2 ,x)))







+++

```
(defun fac (&optional (n 0))
  (if (>= n 0)
      1
      (* n (fac (- n 1)))))
```

+++

## Lexical scoping

(defvar *X*)
   
(defun my-fun(var)
(+ var *X*))

(let ((*X* 5)) (my-fun 4))


Mostly seen with `*standard-output*`


## 


```
(defun test-+ ()
  (and
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

```

+++

```
(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

```

+++


```
(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
```

+++


```
(defmacro check (form)
  `(report-result ,form ',form))

```

+++


```
(macroexpand-1 '(check (= (+ 1 2) 3)))

```

+++

```
(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4))) 


```

+++

```


```

+++
Wants to be able to write
```
(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

```

+++


```

(PROGN
 (REPORT-RESULT (= (+ 1 2) 3) '(= (+ 1 2) 3))
 (REPORT-RESULT (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
 (REPORT-RESULT (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
 

```

+++

```
(defmacro check (&body forms)
  `(progn
     ,(loop for f in forms collect `(report-result ,f ',f))))

```

+++


```
(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))
```

+++
Now we can write
```
(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))


```

+++

```
(defun report-result (result form)
  (progn
    (format t "~:[FAIL~;pass~] ... ~a~%" result form)
    result))

```

+++

```
(defmacro check (&body forms)
  `(and
     ,@(loop for f in forms collect `(report-result ,f ',f))))
```

+++

```
(combine-results
  foo
  bar
  baz)
```
 -> 
```
(let ((result t))
  (if (not foo) (setf result nil))
  (if (not bar) (setf result nil))
  (if (not baz) (setf result nil))
  result)
  
```

+++

```
(defmacro combine-results (&body forms)
    `(let ((result t))
      ,@(loop for f in forms collect `(if (not ,f)  (setf result nil)))
      result))
```

+++
```
(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))
```

+++
```
 (defun test-+ ()
           (check (= (+ 1 2) 3)
             (= (+ 1 2 3) 23)
             (= (+ -1 -3) -4)))
```

+++
```
CL-USER> (test-+)
pass ... (= (+ 1 2) 3)
FAIL ... (= (+ 1 2 3) 23)
pass ... (= (+ -1 -3) -4)
NIL
```

+++
```
(defun test-+ ()
           (check (= (+ 1 2) 3)
             (= (+ 1 2 3) 6)
             (= (+ -1 -3) -4)))
```

+++
```
CL-USER> (test-+)
pass ... (= (+ 1 2) 3)
pass ... (= (+ 1 2 3) 6)
pass ... (= (+ -1 -3) -4)
T
```

+++
```
(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))
```

+++
```
(defun test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))
```
+++
```
(defvar *test-name* nil)

```
+++
```
(defun report-result (result form)
  (progn
    (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
    result))
```
+++
```
(defun test-+ ()
(let ((*test-name* 'test-+))
       (check (= (+ 1 2) 3)
              (= (+ 1 2 3) 6)
              (= (+ -1 -3) -4))))

```
+++
```
(deftest test-+ ()
       (check (= (+ 1 2) 3)
              (= (+ 1 2 3) 6)
              (= (+ -1 -3) -4)))
```
+++
```
(defmacro deftest (name parameters &body body)
   `(defun ,name ,parameters
      (let ((*test-name* ',name))
      ,@body)))
```
+++
```
(defmacro deftest (name parameters &body body)
   `(defun ,name ,parameters
      (let ((*test-name* (append *test-name* (list ',name)) ))
      ,@body)))
```
+++


# Why not lisp?

> I came to Python not because I thought it was a better/acceptable/pragmatic Lisp, but because it was better pseudocode

+++ 

> I think Lisp still has an edge for larger projects and for applications where the speed of the compiled code is important. But Python has the edge (with a large number of students) when the main goal is communication, not programming per se.

+++

> In terms of programming-in-the-large, at Google and elsewhere, I think that language choice is not as important as all the other choices: if you have the right overall architecture, the right team of programmers, the right development process that allows for rapid development with continuous improvement, then many languages will work for you; if you don't have those things you're in trouble regardless of your language choice. 

+++

