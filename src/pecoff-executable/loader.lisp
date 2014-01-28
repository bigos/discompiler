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
  (= 1
     (ldb (byte 1
                (if (eq 'PE32 (optional-header-image-type bytes))
                    31
                    63))
          ilx)))

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
         (import-table-size (multiple-value-bind (d s)
                                (import-directory-table bytes 0)
                              (declare (ignore d))
                              s)))

    (format t "free memory ~S image base ~S ~%" (find-free mem) (image-base bytes))
    ;; Read first page of the file which includes DOS header, PE header, section
    ;; headers etc.

    ;; Fetch Image Base address from PE header and determine if that address is
    ;; available else allocate another area.  (Case of relocation)

    ;; Map the sections into the allocated area
    (allocate-and-load-sections bytes mem)
    ;; TODO memory blocks are still reversed in memory object

    (if (zerop (optional-header-value bytes "Import Table RVA"))
        (princ " zero import RVA detected")
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
               (format t "import directory table ~S~%" idt)
             ;; need to check why malformed string is being returned
               (format t "import address table ~X~%" (struct-value "ImportAddressTableRVA" idt))
               (format t "~S ~%" (library-name mem bytes idt))
               (format t "import lookup table ~X~%" (rva-addr
                                                     (struct-value "ImportLookupTableRVA" idt)
                                                     bytes))
               (format t "~%")
               (imported-function-names mem bytes idt)
               )))
    (princ " after loop ")
    ;; don't show it for now, less problems with large files
    ;; (format t "resource table RVA~%~S~%"
    ;;         (get-rva-table-bytes bytes
    ;;                              mem
    ;;                              "Resource Table RVA"
    ;;                              "Resource Table Size"))
    (if (zerop (optional-header-value bytes "IAT RVA"))
        (princ " zero IAT rva detected ")
        (format t "IAT RVA~%~S~%"
                (get-rva-table-bytes bytes mem "IAT RVA" "IAT Size" )))


    ;; Read information from import table and load the DLLs

    ;; Resolve the function addresses and create Import Address Table (IAT).

    ;; Create initial heap and stack using values from PE header.

    ;; Create main thread and start the process.

    mem))


;; page 100 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun export-directory-table (bytes offset)
  (let ((elements '((+long+ "ExportFlags")
                    (+long+ "TimeDateStamp")
                    (+short+ "MajorVersion")
                    (+short+ "MinorVersion")
                    (+long+ "NameRVA")
                    (+long+ "OrdinalBase")
                    (+long+ "AddressTableEntries")
                    (+long+ "NumberOfNamePointers")
                    (+long+ "ExportAddressTableRVA")
                    (+long+ "NamePointerRVA")
                    (+long+ "OrdinalTableRVA"))))
    (multiple-value-bind (data structure-size)
        (c-structure-values bytes elements offset)
      (values data structure-size))))

(defun in-export-tablep (a bytes)
   (< (optional-header-value bytes "Export Table RVA")
        a
        (+ (optional-header-value bytes "Export Table RVA")
           (optional-header-value bytes "Export Table Size"))))

(defun export-address-table (bytes memory edt)
  (loop for ate from 0 to (1- (struct-value "AddressTableEntries" edt))
     for offset = (rva-addr  (+ (struct-value "ExportAddressTableRVA" edt)
                                (* ate  +long+))
                             bytes)
     for a = (bytes-to-type-int (get-allocated-bytes memory offset 4))
     collect
       (rva-addr a bytes)))

(defun name-pointer-table (bytes memory edt)
  (loop for npe from 0 to (1- (struct-value "NumberOfNamePointers" edt))
     for y = (rva-addr (+ (struct-value "NamePointerRVA" edt) (* npe +long+))
                       bytes)
     for a = (bytes-to-type-int (get-allocated-bytes memory y 4))
     collect
       (get-allocated-string memory (rva-addr a bytes))))

(defun export-ordinal-table (bytes memory edt)
  (loop for npe from 0 to (1- (struct-value "NumberOfNamePointers" edt))
     for y = (rva-addr (+ (struct-value "OrdinalTableRVA" edt) (* npe +short+))
                       bytes)
     for a =  (bytes-to-type-int
               (get-allocated-bytes memory y +short+))
     collect (cons a (+ a (struct-value "OrdinalBase" edt)))))

(defun address-to-code (export-list table-pos)
  "address of execuable code for ordinal table entry"
  (let ((name (nth table-pos (nth 1 export-list)))
        (ordinal (nth table-pos (nth 2 export-list))))
    (format t "  ordinal # ~s    export address table index ~s  name ~S~%"
            (cdr ordinal) (car ordinal) name)
    (int-to-hex (nth (car ordinal) (nth 0 export-list) ))
    ))

(defun exports (bytes memory)
  (if (zerop (optional-header-value bytes "Export Table RVA"))
      (princ " zero size export table ")
      (let* ((edt (export-directory-table
                   (get-rva-table-bytes bytes memory "Export Table RVA"
                                        "Export Table Size") 0))
             (address-table (export-address-table bytes memory edt))
             (name-table (name-pointer-table bytes memory edt))
             (ordinal-table (export-ordinal-table bytes memory edt))
             (results))

        (format t "memory blocks ~S~%" (blocks memory))
        (format t "~S  ~%~S ~S~%entries ~S names ~S address table hex addr  ~S~%"
                edt
                (get-allocated-string  memory
                                       (rva-addr
                                        (struct-value
                                         "NameRVA"
                                         edt)
                                        bytes))
                (struct-value "OrdinalBase" edt)
                (struct-value "AddressTableEntries" edt)
                (struct-value "NumberOfNamePointers" edt)
                (hex-rva-addr  (struct-value "ExportAddressTableRVA" edt) bytes))
        (format t "~&address table entries~%")
        (format t "item addr, val, rva, forwarding, result~%~s~%"
                address-table)
        (format t "~&name pointer entries~%~S~%"
                name-table)
        (format t "~&Export ordinal table~%~S~%"
                ordinal-table)
        (setf results (list address-table name-table ordinal-table))
        )))
