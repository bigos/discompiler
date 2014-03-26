(in-package :discompiler)

(defun loader (bytes)
  (let ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001))
        (my-module (make-module) ))
    (allocate-and-load-sections bytes mem)
    (if (zerop (optional-header-value bytes "Import Table RVA"))
        (princ " zero import RVA detected")
        (imported-functions bytes mem))
    (if (zerop (optional-header-value bytes "IAT RVA"))
        (princ " zero IAT rva detected ")
        ;; (format t "IAT RVA~%~S~%" (get-rva-table-bytes bytes mem "IAT RVA" "IAT Size" ))
        )

    ;; Read information from import table and load the DLLs
    ;; Resolve the function addresses and create Import Address Table (IAT).
    ;; Create initial heap and stack using values from PE header.
    ;; Create main thread and start the process.
    (values
     mem
     12345)))

(defun loader_memory (loader-structure)
  )


;; Windows Internals Part 1 (6th Edition)
;; page 234
;; Early Process Initialization
;;
;; page 238
;; Loaded Module Database
;;
;; page 242
