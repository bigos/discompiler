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
  (let* ((ates (struct-value "AddressTableEntries" edt))
         (res (make-array `(,ates) :element-type 'integer)))
    (loop for ate from 0 below ates
       for offset = (rva-addr-in-struct "ExportAddressTableRVA" edt bytes (* ate +long+))
       for a = (bytes-to-type-int (get-allocated-bytes memory offset 4))
       do
         (setf (aref res ate) (rva-addr a bytes)))
    res))

(defun name-pointer-table (bytes memory edt)
  (let* ((npes (struct-value "NumberOfNamePointers" edt))
         (res (make-array `(,npes))))
    (loop for npe from 0 below npes
       for y = (rva-addr-in-struct "NamePointerRVA" edt bytes (* npe +long+))
       for a = (bytes-to-type-int (get-allocated-bytes memory y 4))
       do
         (setf (aref res npe)
               (get-allocated-string memory (rva-addr a bytes))))
    res))

(defun export-ordinal-table (bytes memory edt)
  (let* ((npes (struct-value "NumberOfNamePointers" edt))
         (res (make-array `(,npes))))
    (loop for npe from 0 below npes
       for y = (rva-addr-in-struct "OrdinalTableRVA" edt bytes (* npe +short+))
       for a = (bytes-to-type-int
                (get-allocated-bytes memory y +short+))
       do (setf (aref res npe) (cons a (+ a (struct-value "OrdinalBase" edt)))))
    res))

(defun address-to-code (export-list table-pos)
  "address of execuable code for ordinal table entry"
  (let* ((name (elt (elt export-list 1) table-pos))
         (ord (elt (elt export-list 2) table-pos))
         (ordinal (cdr ord))
         (addr-index (car ord)))
    ;;(format t "  ordinal # ~s    export address table index ~s  name ~S~%" ordinal addr-index name)
    (int-to-hex (elt (elt export-list 0) addr-index))))

(defun ordinal-name (export-list ord)
  (loop for i from 0 below (length (elt export-list 2))
     for o = (aref (elt export-list 2) i)
     for x from 0
     for ordinal = (cdr o)
     until (eq ord ordinal)
     finally (return (elt (elt export-list 1) x))))

(defun ordinal-code-address (export-list ord)
  (loop for i from 0 below (length (nth 2 export-list))
     for o = (aref  (nth 2 export-list) i)
     for x from 0
     for ordinal = (cdr o)
     for index = (car o)
     until (eq ord ordinal)
     finally (return (elt (elt export-list 0) index))))

(defun ordinal-names (export-list)
  (let ((res (make-array (list (length (nth 2 export-list))))))
      (loop for i below (length (nth 2 export-list))
         for o = (aref (nth 2 export-list) i)
         for x from 0
         for ordinal = (cdr o)
         do
           (setf (aref res i) (cons ordinal (elt (nth 1 export-list) x))))
      res))

(defun file-export-list (file memory)
  (let* ((bytes (file-to-bytes file)))
    (exports bytes memory)))

(defun exports (bytes memory)
  (declare (optimize (debug 3)))
  (if (zerop (optional-header-value bytes "Export Table RVA"))
      (princ " zero size export table ")
      (let* ((edt (export-directory-table
                   (get-rva-table-bytes bytes
                                        memory
                                        "Export Table RVA"
                                        "Export Table Size")
                   0))
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
