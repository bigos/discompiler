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
  (rva-addr
   (+ (struct-value struct-name struct)
      offset)
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
         (format t "import directory table ~S~%" idt)
         (format t "import address table ~X~%" (struct-value "ImportAddressTableRVA" idt))
         (format t "~S ~%" (library-name mem bytes idt))
         (format t "import lookup table ~X~%"
                 (rva-addr-in-struct  "ImportLookupTableRVA" idt bytes ))
         (format t "~%")
         (imported-function-names mem bytes idt))))

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
     for offset = (rva-addr-in-struct "ExportAddressTableRVA" edt bytes (* ate +long+))

     for a = (bytes-to-type-int (get-allocated-bytes memory offset 4))
     collect
       (rva-addr a bytes)))

(defun name-pointer-table (bytes memory edt)
  (loop for npe from 0 to (1- (struct-value "NumberOfNamePointers" edt))
     for y = (rva-addr-in-struct "NamePointerRVA" edt bytes (* npe +long+))

     for a = (bytes-to-type-int (get-allocated-bytes memory y 4))
     collect
       (get-allocated-string memory (rva-addr a bytes))))

(defun export-ordinal-table (bytes memory edt)
  (declare (optimize (speed 0) (space 1) (compilation-speed 0) (debug 3)))
  (loop for npe from 0 to (1- (struct-value "NumberOfNamePointers" edt))
     for y = (rva-addr-in-struct "OrdinalTableRVA" edt bytes (* npe +short+))
     for a = (bytes-to-type-int
              (get-allocated-bytes memory y +short+))
     collect (cons a (+ a (struct-value "OrdinalBase" edt)))))

(defun address-to-code (export-list table-pos)
  "address of execuable code for ordinal table entry"
  (let* ((name (nth table-pos (nth 1 export-list)))
         (ord (nth table-pos (nth 2 export-list)))
         (ordinal (cdr ord))
         (addr-index (car ord)))
    (format t "  ordinal # ~s    export address table index ~s  name ~S~%"             ordinal addr-index name)
    (int-to-hex (nth addr-index (nth 0 export-list)))))

(defun ordinal-name (export-list ord)
  (loop for o in (nth 2 export-list)
     for x from 0
     for ordinal = (cdr o)
     until (eq ord ordinal)
     finally (return (nth x (nth 1 export-list)))))

(defun ordinal-code-address (export-list ord)
  (loop for o in (nth 2 export-list)
     for x from 0
     for ordinal = (cdr o)
     for index = (car o)
     until (eq ord ordinal)
     finally (return (nth index (nth 0 export-list)))))

(defun ordinal-names (export-list)
  (loop for o in (nth 2 export-list)
     for x from 0
     for ordinal = (cdr o)
     collect (cons ordinal (nth x (nth 1 export-list)))))

(defun file-export-list (file)
  (let* ((bytes (file-to-bytes file))
         (memory (loader bytes)))
    (exports bytes memory)))

(defun exports (bytes memory)
  (if (zerop (optional-header-value bytes "Export Table RVA"))
      (princ " zero size export table ")
      (let* ((edt (export-directory-table
                   (get-rva-table-bytes bytes memory "Export Table RVA"
                                        "Export Table Size") 0))
             (address-table (export-address-table bytes memory edt))
             (name-table (name-pointer-table bytes memory edt))
             (ordinal-table (export-ordinal-table bytes memory edt)))

        ;; (format t "memory blocks ~S~%" (blocks memory))
        ;; (format t "~S  ~%~S ~S~%entries ~S names ~S address table hex addr  ~S~%"
        ;;         edt
        ;;         (get-allocated-string  memory
        ;;                                (rva-addr
        ;;                                 (struct-value
        ;;                                  "NameRVA"
        ;;                                  edt)
        ;;                                 bytes))
        ;;         (struct-value "OrdinalBase" edt)
        ;;         (struct-value "AddressTableEntries" edt)
        ;;         (struct-value "NumberOfNamePointers" edt)
        ;;         (hex-rva-addr  (struct-value "ExportAddressTableRVA" edt) bytes))
        ;; (format t "~&address table entries~%")
        ;; (format t "item addr, val, rva, forwarding, result~%~s~%" address-table)
        ;; (format t "~&name pointer entries~%~S~%" name-table)
        ;; (format t "~&Export ordinal table~%~S~%" ordinal-table)
        (list address-table name-table ordinal-table)
        )))
