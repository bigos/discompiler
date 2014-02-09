(in-package :discompiler)

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

(defun in-export-tablep (addr bytes)
  (< (optional-header-value bytes "Export Table RVA")
     addr
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
    ;;(format t "  ordinal # ~s    export address table index ~s  name ~S~%" ordinal addr-index name)
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
