(in-package :discompiler)

;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~&~:[FAIL~;pass~] ... ~a: ~s~%" result *test-name* form)
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 7)
    (= (+ -1 -3) -4)))

(deftest test-/ ()
  (check 
    (= (/ 6 1) 2)
    (= (/ 4 2) 2)))

(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-/)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest test-hex-bin ()
  (check
    (equal "00000000" (int-to-bin 0))
    (equal "11111111" (int-to-bin 255))
    (equal "10101010" (int-to-bin 170))))

(deftest test-mod-rm ()
  (check
    (equal '(1 3 5) (modrmreg-vals "6b"))
    (equal '(0 0 0) (modrmreg-vals (int-to-hex 0)))
    (equal '(0 0 7) (modrmreg-vals "38"))
    (equal '(0 7 0) (modrmreg-vals "07"))
    (equal '(2 4 6) (modrmreg-vals (int-to-hex 180)))
    (equal '(3 7 7) (modrmreg-vals (int-to-hex 255)))
    (equal '(3 2 1) (modrmreg-vals "ca"))
    (= 1 (mod-part "6b"))
    (= 3 (rm-part "6b"))
    (= 5 (reg-part "6b"))))

(deftest test-sib ()
  (check
    (= 1 (ss-part "5d"))
    (= 3 (index-part "5d"))
    (= 5 (base-part "5d"))
    (= 2 (ss-part "a6"))
    (= 4 (index-part "a6"))
    (= 6 (base-part "a6"))))

(defun test-mod-vals ()
  (combine-results
    (test-hex-bin)
    (test-sib)))

