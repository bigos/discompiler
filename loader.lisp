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
  (let ((found-free))
    (dolist (chunk allocated)
      (if (eq (car chunk) first-available)
          (setf first-available (1+ (cdr chunk)))
          (progn
            (push (cons first-available (1- (car chunk))) found-free)
            (setf first-available (1+ (cdr chunk))))))
    (when (< (cdar (last allocated)) last-available)
      (push (cons (1+ (cdar (last allocated))) last-available) found-free))
    (reverse found-free)))

(defclass exec ()
  (preferred-address
   obtained-address))
