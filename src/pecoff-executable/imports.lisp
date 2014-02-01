(in-package :discompiler)

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
  (concatenate 'string ""
               (loop for offset from 0 by 1
                  for c = (get-allocated mem
                                         (rva-addr-in-struct "NameRVA"
                                                             directory-table
                                                             bytes
                                                             offset))
                  until (zerop c)
                  collecting (code-char c))))

(defun import-by-ordinalp (bytes ilx)
  (= 1 (ldb (byte 1
                  (if (eq 'PE32 (optional-header-image-type bytes))
                      31
                      63))
            ilx)))

(defun imported-function-hint (mem bytes ilx)
  (bytes-to-type-int
   (get-allocated-bytes mem
                        (rva-addr ilx bytes) 2)))

(defun imported-function-name (mem bytes ilx offset)
  (concatenate 'string ""
               (loop for offset from 2 by 1
                  for c = (get-allocated mem
                                         (rva-addr (+ offset ilx)
                                                   bytes))
                  while (not (zerop c))
                  collecting (code-char c))))

(defun imported-function-names (mem bytes imp-dir-rva offset)
  (declare (optimize (speed 0) (space 1) (compilation-speed 0) (debug 3)))
  (loop for il from imp-dir-rva by 4
     for ilx = (bytes-to-type-int (get-allocated-bytes mem (rva-addr il bytes) 4))
     while (not (zerop ilx))
     collect
       (list il  ilx
             (if (import-by-ordinalp bytes ilx)
                 (ldb (byte 16 0) ilx)
                 (cons
                  (imported-function-hint mem bytes ilx)
                  (imported-function-name mem bytes ilx offset)
                  )))))

(defun imported-functions (bytes mem)
  (let ((import-table-size (multiple-value-bind (d s)
                               (import-directory-table bytes 0)
                             (declare (ignore d)) s)))
    (loop
       for offset from 0 by import-table-size
       for idt = (import-directory-table
                  (get-rva-table-bytes bytes
                                       mem
                                       "Import Table RVA"
                                       "Import Table Size")
                  offset)
       for imp-dir-rva = (struct-value "ImportLookupTableRVA" idt)
       until (zerop imp-dir-rva)
       collect
         (list
          (library-name mem bytes idt)
          (imported-function-names mem bytes imp-dir-rva offset))
         )))
