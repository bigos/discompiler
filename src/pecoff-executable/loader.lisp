(in-package :discompiler)

;;; !!! READ THE FINAL COMMENTS IN HEADER-INFO.LISP !!!

(defun loader (bytes)
  (let* ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001))
         (rdata)
         )

    (format t "free memory ~S image base ~S ~%" (find-free mem) (image-base bytes))
    ;; Read first page of the file which includes DOS header, PE header, section
    ;; headers etc.

    ;; Fetch Image Base address from PE header and determine if that address is
    ;; available else allocate another area.  (Case of relocation)

    ;; Map the sections into the allocated area
    (allocate-and-load-sections bytes mem)

    (setf rdata (data  (nth 2 (blocks mem))))
    (format t ".rdata section~%~S~%" rdata)
    ;; Read information from import table and load the DLLs

    ;; Resolve the function addresses and create Import Address Table (IAT).

    ;; Create initial heap and stack using values from PE header.

    ;; Create main thread and start the process.

    mem))

;;; use this in repl
;;; (setf *memory* (loader *bytes*))
