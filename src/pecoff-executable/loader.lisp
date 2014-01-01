(in-package :discompiler)

;;; !!! READ THE FINAL COMMENTS IN HEADER-INFO.LISP !!!

(defun get-rva-table-bytes (bytes mem name size)
  (get-allocated-bytes mem
                       (rva-addr
                        (optional-header-value bytes name)
                        bytes)
                       (optional-header-value bytes size )))

(defun import-directory-table (bytes offset)
  (let ((elements '((+long+ "ImportLookupTableRVA")
                    (+long+ "TimeDate")
                    (+long+ "ForwarderChain")
                    (+long+ "NameRVA") ;addr of imported library name
                    (+long+ "ImportAddressTableRVA"))))
    (multiple-value-bind (data structure-size)
        (c-structure-values bytes elements offset)
      (values data structure-size))))

(defun library-name (mem bytes directory-table)
  (declare (optimize (speed 0) (space 1) (compilation-speed 0) (debug 3)))
  (concatenate 'string ""
               (loop for offset from 0 by 1
                  for c = (get-allocated mem (rva-addr
                                              (+ offset
                                                 (struct-value "NameRVA"
                                                               directory-table))
                                              bytes ))
                  until (zerop c)
                  collecting  (code-char c))))

(defun import-by-ordinalp (bytes ilx)
  (if (= 1
         (ldb (byte 1
                    (if (eq 'PE32 (optional-header-image-type bytes))
                        31
                        63))
              ilx))
      T
      nil))

(defun imported-function-names (mem bytes imp-dir-tbl)
  (declare (optimize (speed 0) (space 1) (compilation-speed 0) (debug 3)))
  (loop for il from (struct-value "ImportLookupTableRVA" imp-dir-tbl) by 4
     for ilx = (bytes-to-type-int (get-allocated-bytes mem (rva-addr il bytes) 4))
     while (not (zerop ilx))
     do
       (format t "~&function data ~x  ~x ~S~%" il  ilx
               (if (import-by-ordinalp bytes ilx)
                   (ldb (byte 16 0) ilx)
                   (concatenate 'string
                                (format nil " ~S <-hint   "
                                        (bytes-to-type-int (get-allocated-bytes mem (rva-addr ilx bytes) 2)))
                                (loop for offset from 2 by 1
                                   for c = (get-allocated mem
                                                          (rva-addr
                                                           (+ offset ilx )
                                                           bytes))
                                   while (not (zerop c))
                                   collecting (code-char c)))))))

(defun loader (bytes)
  (let* ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001))
         (rdata) (import-table-size 20) (il)
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
       for offset from 0 by import-table-size
       for idt = (import-directory-table
                  (get-rva-table-bytes bytes
                                       mem
                                       "Import Table RVA"
                                       "Import Table Size") offset)
       until (zerop  (struct-value "ImportLookupTableRVA" idt))
       do
         (format t "import directory table ~S~%" idt)
       ;; need to check why malformed string is being returned
         (format t "import address table ~X~%" (struct-value "ImportAddressTableRVA" idt))
         (format t "~S ~%" (library-name mem bytes idt))
         (format t "import lookup table ~X~%" (rva-addr
                                               (struct-value "ImportLookupTableRVA" idt)
                                               bytes))
         (format t "~%")
         (imported-function-names mem bytes idt)
         )
    ;; don't show it for now, less problems with large files
    ;; (format t "resource table RVA~%~S~%"
    ;;         (get-rva-table-bytes bytes
    ;;                              mem
    ;;                              "Resource Table RVA"
    ;;                              "Resource Table Size"))

    (format t "IAT RVA~%~S~%"
            (get-rva-table-bytes bytes mem "IAT RVA" "IAT Size" ))


    ;; Read information from import table and load the DLLs

    ;; Resolve the function addresses and create Import Address Table (IAT).

    ;; Create initial heap and stack using values from PE header.

    ;; Create main thread and start the process.

    mem))

;;; use this in repl
;;; (setf *memory* (loader *bytes*))
