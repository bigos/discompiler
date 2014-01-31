(in-package :discompiler)

(defun library-name (mem bytes directory-table)
  (concatenate 'string ""
               (loop for offset from 0 by 1
                  for c = (get-allocated mem
                                         (rva-addr-in-struct "NameRVA"
                                                             directory-table
                                                             bytes
                                                             offset))
                  until (zerop c)
                  collecting (code-char c))))


(defun loader-data (bytes mem import-table-size)
  ;; (format t "import table RVA directory bytes~%~S~%"
  ;;         (get-rva-table-bytes bytes
  ;;                              mem
  ;;                              "Import Table RVA"
  ;;                              "Import Table Size"))
  (loop
     for offset from 0 by import-table-size
     for idt = (import-directory-table
                (get-rva-table-bytes bytes
                                     mem
                                     "Import Table RVA"
                                     "Import Table Size")
                offset)
     until (zerop  (struct-value "ImportLookupTableRVA" idt))
     do
       ;; (format t "~%import directory table ~S~%" idt)
       ;; (format t "import address table ~X~%" (struct-value "ImportAddressTableRVA" idt))
       (format t "~%~S ~%" (library-name mem bytes idt))
       ;; (format t "import lookup table ~X~%"
       ;;         (rva-addr-in-struct  "ImportLookupTableRVA" idt bytes ))
       ;; (format t "~%")
       (format t "~&imported functions ~S~%"
               (imported-function-names mem bytes idt offset))
       ))

(defun loader (bytes)
  (let* ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001))
         (import-table-size (multiple-value-bind (d s)
                                (import-directory-table bytes 0)
                              (declare (ignore d)) s)))
    (allocate-and-load-sections bytes mem)
    ;; TODO memory blocks are still reversed in memory object
    (if (zerop (optional-header-value bytes "Import Table RVA"))
        (princ " zero import RVA detected")
        (progn
          (format t "import-table-size ~s~%" import-table-size)
          (loader-data bytes mem import-table-size)))
    (if (zerop (optional-header-value bytes "IAT RVA"))
        (princ " zero IAT rva detected ")
        (format t "IAT RVA~%~S~%"
                (get-rva-table-bytes bytes mem "IAT RVA" "IAT Size" )))

    ;; Read information from import table and load the DLLs
    ;; Resolve the function addresses and create Import Address Table (IAT).
    ;; Create initial heap and stack using values from PE header.
    ;; Create main thread and start the process.
    mem))
