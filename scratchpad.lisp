;; This file is for temporary Lisp code, for situations where REPL is not convenient enough
;; Once the code matures, it should go to source folder.

(defun investigate ()
  (declare (debug 3))
  (let* ((file "~/discompiler/SampleExecutables/PE/myfavlibrary.exe")
         (bytes (file-to-bytes file))
         (mem (loader bytes))
         ;; (imports (imported-functions bytes mem))
         )
    (let ((import-table-size (multiple-value-bind (d s)
                                 (import-directory-table bytes 0)
                               (declare (ignore d)) s)))
      (unless (zerop (optional-header-value bytes "Import Table RVA"))
        (loop
           for offset from 0 by import-table-size
           for rva-bytes = (get-rva-table-bytes bytes
                                                mem
                                                "Import Table RVA"
                                                "Import Table Size")
           for idt = (import-directory-table rva-bytes offset)
           for imp-dir-rva = (struct-value "ImportLookupTableRVA" idt)
           until (zerop imp-dir-rva)
           do (when (eq offset 340)
                (cerror "as normal" "investigate me"))
           collect
             (list
              (library-name mem bytes idt)
              (imported-function-names mem bytes imp-dir-rva))
             )))
    (format t "~&~A~%" (nth 17 imports))))

;;;
