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

(defun rva-addr-in-struct (struct-name struct bytes &optional (offset 0))
  (rva-addr (+ (struct-value struct-name struct) offset)
            bytes))

(defun library-name (mem bytes directory-table)
  (declare (optimize (speed 0) (space 1) (compilation-speed 0) (debug 3)))
  (concatenate 'string ""
               (loop for offset from 0 by 1
                  for c = (get-allocated mem
                                         (rva-addr-in-struct "NameRVA"
                                                             directory-table
                                                             bytes
                                                             offset))
                  until (zerop c)
                  collecting  (code-char c))))

(defun import-by-ordinalp (bytes ilx)
  (= 1 (ldb (byte 1
                  (if (eq 'PE32 (optional-header-image-type bytes))
                      31
                      63))
            ilx)))

(defun imported-function-hint (mem bytes ilx)
  (format nil " ~S <-hint   "
          (bytes-to-type-int
           (get-allocated-bytes mem
                                (rva-addr ilx bytes) 2))))

(defun imported-function-name (mem bytes ilx offset)
  (concatenate 'string ""
               (loop for offset from 2 by 1
                  for c = (get-allocated mem
                                         (rva-addr (+ offset ilx)
                                                   bytes))
                  while (not (zerop c))
                  collecting (code-char c))))

(defun imported-function-names (mem bytes imp-dir-tbl offset)
  (declare (optimize (speed 0) (space 1) (compilation-speed 0) (debug 3)))
  (loop for il from (struct-value "ImportLookupTableRVA" imp-dir-tbl) by 4
     for ilx = (bytes-to-type-int (get-allocated-bytes mem (rva-addr il bytes) 4))
     while (not (zerop ilx))
     do
       (format t "~&function data ~x  ~x ~S~%" il  ilx
               (if (import-by-ordinalp bytes ilx)
                   (ldb (byte 16 0) ilx)
                   (concatenate 'string
                                (imported-function-hint mem bytes ilx)
                                (imported-function-name mem bytes ilx offset)
                                )))))

(defun loader-data (bytes mem import-table-size)
  (progn
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
         (format t "~%import directory table ~S~%" idt)
         (format t "import address table ~X~%" (struct-value "ImportAddressTableRVA" idt))
         (format t "~S ~%" (library-name mem bytes idt))
         (format t "import lookup table ~X~%"
                 (rva-addr-in-struct  "ImportLookupTableRVA" idt bytes ))
         (format t "~%")
         (imported-function-names mem bytes idt offset))))

(defun loader (bytes)
  (let* ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001))
         (import-table-size (multiple-value-bind (d s)
                                (import-directory-table bytes 0)
                              (declare (ignore d)) s)))
    (allocate-and-load-sections bytes mem)
    ;; TODO memory blocks are still reversed in memory object
    (if (zerop (optional-header-value bytes "Import Table RVA"))
        (princ " zero import RVA detected")
        (loader-data bytes mem import-table-size))
    (if (zerop (optional-header-value bytes "IAT RVA"))
        (princ " zero IAT rva detected ")
        (format t "IAT RVA~%~S~%"
                (get-rva-table-bytes bytes mem "IAT RVA" "IAT Size" )))

    ;; Read information from import table and load the DLLs
    ;; Resolve the function addresses and create Import Address Table (IAT).
    ;; Create initial heap and stack using values from PE header.
    ;; Create main thread and start the process.
    mem))
