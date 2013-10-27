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

(defun load-in-memory (bytes)
  "simulate loading executable in memory")

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

(defun find-free-block (allocated first-available last-available size)
  (let ((found-free) (last-allocated))
    (loop for allocated-block in allocated
       do
         (when (not (eq (car allocated-block) first-available))
           (push (cons first-available (1- (car allocated-block))) found-free))
         (setf first-available (1+ (cdr allocated-block)))
       until
         (>= (- (cdar found-free) (caar found-free)) size))
    (setf last-allocated (cdar (last allocated)))
    (if (>= (- (cdar found-free) (caar found-free)) size)
        (caar found-free)
        (if (>= (- last-available (1+ last-allocated)) size)
            (1+ last-allocated)))))

(defclass exec ()
  (preferred-address
   obtained-address))
