(in-package :discompiler)

;;; !!! READ THE FINAL COMMENTS IN HEADER-INFO.LISP !!!

(defun get-rva-bytes (bytes mem name size)
  (get-allocated-bytes mem
                       (rva-addr
                        (optional-header-value bytes name)
                        bytes)
                       (optional-header-value bytes size )))

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
    ;; TODO memory blocks are still reversed in memory object

    (setf rdata (data  (nth 2 (blocks mem))))
    (format t "beginning of .rdata section~%~S~%" (subseq  rdata 0 #x130))

    (format t "import table RVA directory bytes~%~S~%"
            (get-rva-bytes bytes mem "Import Table RVA" "Import Table Size"))

    (format t "resource table RVA~%~S~%" (get-rva-bytes bytes mem "Resource Table RVA" "Resource Table Size"))

    (format t "IAT RVA~%~S~%"
            (get-rva-bytes bytes mem "IAT RVA" "IAT Size" ))


    ;; Read information from import table and load the DLLs

    ;; Resolve the function addresses and create Import Address Table (IAT).

    ;; Create initial heap and stack using values from PE header.

    ;; Create main thread and start the process.

    mem))

;;; use this in repl
;;; (setf *memory* (loader *bytes*))
