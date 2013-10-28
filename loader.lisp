(in-package :discompiler)


#|
Read first page of the file which includes DOS header, PE header, section
headers etc.

Fetch Image Base address from PE header and determine if that address is
available else allocate another area.  (Case of relocation)

Map the sections into the allocated area

Read information from import table and load the DLLs

Resolve the function addresses and create Import Address Table (IAT).

Create initial heap and stack using values from PE header.

Create main thread and start the process.
|#

(defparameter *allocated*)

(defun load-in-memory (bytes preferred-addr)
  "simulate loading executable in memory"
  (if (is-block-available *allocated* preferred-addr (array-total-size bytes))
      ;;allocate preferred
      ;;if available
      ;;allocate available
      ;;else raise error
      ))

(defun find-free (allocated first-available last-available)
  (let ((found-free) (last-allocated))
    (dolist (allocated-block allocated)
      (when (not (eq (car allocated-block) first-available))
        (push (cons first-available (1- (car allocated-block))) found-free))
      (setf first-available (1+ (cdr allocated-block))))
    (setf last-allocated (cdar (last allocated)))
    (when (< last-allocated last-available)
      (push (cons (1+ last-allocated) last-available) found-free))
    (reverse found-free)))

(defun is-block-available (allocated addr size)
  (let ((result))
      (dolist (allocated-block allocated)
        (if (or (and (>= addr (car allocated-block))
                     (<= addr (cdr allocated-block)))
                (and (>= (+ addr size -1) (car allocated-block))
                     (<= (+ addr size -1) (cdr allocated-block))))
            (setf result T)))
      (not result)))

;; (defun find-free-block (allocated first-available last-available required-size)
;;   (dolist (allocated-block allocated)
;;     (if (>= (- (car allocated-block) first-available)
;;             required-size)
;;         (return first-available)
;;         (setf first-available (1+ (cdr allocated-block)))))
;;   (if (>= (- (incf last-available) first-available)
;;           required-size)
;;       first-available))

;; First, transform it to have really some code duplication:
;; (defun find-free-block (allocated first-available last-available required-size)
;;   (dolist (allocated-block allocated)
;;     (when (>= (- (car allocated-block) first-available) required-size)
;;       (return-from find-free-block first-available))
;;     (setf first-available (1+ (cdr allocated-block))))
;;   (when (>= (- (incf last-available) first-available) required-size)
;;     (return-from find-free-block first-available)))

;; Then apply the standard factorization, but using flet:
(defun find-free-block (allocated first-available last-available required-size)
  (flet ((common-code (address)
           (when (>= (- address first-available) required-size)
             (return-from find-free-block  first-available))))
    (dolist (allocated-block allocated)
      (common-code (car allocated-block))
      (setf first-available (1+ (cdr allocated-block))))
    (common-code (incf last-available))))

(defclass exec ()
  (preferred-address
   obtained-address))
