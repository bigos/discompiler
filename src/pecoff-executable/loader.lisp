(in-package :discompiler)

;;; !!! READ THE FINAL COMMENTS IN HEADER-INFO.LISP !!!

(defun get-rva-table-bytes (bytes mem name size)
  (get-allocated-bytes mem
                       (rva-addr
                        (optional-header-value bytes name)
                        bytes)
                       (optional-header-value bytes size )))

;; TODO wrong assumptions hence offset is zero
;; I need to make offset based of memory locastions not file locations
(defun import-directory-table (bytes offset)
  (let ((elements '((+long+ "ImportLookupTableRVA") ;table of addresses for imported function hint tables
                    (+long+ "TimeDate")
                    (+long+ "ForwarderChain")
                    (+long+ "NameRVA") ;addr of imported library name
                    (+long+ "ImportAddressTableRVA"))))
    (multiple-value-bind (data structure-size)
        (c-structure-values bytes elements offset)
      (values data structure-size))))

(defun library-name (mem bytes directory-table)

  ( loop for offset from 0 by 1
         for c = (get-allocated *memory* (rva-addr
                                          (+ offset
                                             (nth 2 (nth 3 directory-table)))
                                          bytes ))
         collecting c
         until (zerop c)
         do
         (format t "~c " (code-char c)))
  )

(defun loader (bytes)
  (let* ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001))
         (rdata) (import-table-size 20)
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
            (get-rva-table-bytes bytes
                                 mem
                                 "Import Table RVA"
                                 "Import Table Size"))

    (loop
       for y from 0 by import-table-size
       for x = (import-directory-table
                (get-rva-table-bytes bytes
                                     mem
                                     "Import Table RVA" "Import Table Size")
                y)
       until (zerop  (nth 2 (car x)))
       do
         (format t "import directory table ~S~%~%" x)
         (library-name mem bytes x )
         (format t "~%")


         )

    (format t "resource table RVA~%~S~%"
            (get-rva-table-bytes bytes
                                 mem
                                 "Resource Table RVA"
                                 "Resource Table Size"))

    (format t "IAT RVA~%~S~%"
            (get-rva-table-bytes bytes mem "IAT RVA" "IAT Size" ))


    ;; Read information from import table and load the DLLs

    ;; Resolve the function addresses and create Import Address Table (IAT).

    ;; Create initial heap and stack using values from PE header.

    ;; Create main thread and start the process.

    mem))

;;; use this in repl
;;; (setf *memory* (loader *bytes*))
