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

(defparameter *allocated* nil)

(defun load-in-memory (bytes preferred-addr)
  "simulate loading executable in memory"
  (let ((size (array-total-size bytes)))
    (if (is-block-available *allocated* preferred-addr (array-total-size bytes))
        nil
        ;;(allocate-block preferred-addr size)
        ;;if available
        ;;allocate available
        ;;else raise error
        )))

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

(defun find-free-block (allocated start end size)
  (dolist (avail (find-free allocated start end))
    (if (>= (1+ (- (cdr avail) (car avail))) size)
        (return avail))))

(defun allocate-block (allocated start end size)
  (let ((found) (res allocated))
    (if (setf found (car (find-free-block allocated start end size)))
      (progn
        (push (cons found (+ found size -1 )) res)
        (sort res #'< :key #'car)
        allocated)
      allocated)))

(defclass exec ()
  (preferred-address
   obtained-address))
