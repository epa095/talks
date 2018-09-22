(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name)) ))
       ,@body)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  `(let ((result t))
     ,@(loop for f in forms collect `(if (not ,f)  (setf result nil)))
     result))

(defun report-result (result form)
  (progn
    (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
    result))



;; Example code:
(defun fac (&optional (n 0))
  (if (<= n 0)
      1
      (* n (fac (- n 1)))))

;; Tests:

(deftest test-+ ()
  (check (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(deftest test-fac ()
  (check (= (fac 0) 1)
    (= (fac 1) 1)
    (= (fac -12) 1)
    (= (fac 3) 6)))


(deftest test-basic-math ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-math ()
  (combine-results
    (test-basic-math)
    (test-fac)))
