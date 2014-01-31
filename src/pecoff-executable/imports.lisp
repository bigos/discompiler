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
     collect
       (list il  ilx
               (if (import-by-ordinalp bytes ilx)
                   (ldb (byte 16 0) ilx)
                   (concatenate 'string
                                (imported-function-hint mem bytes ilx)
                                (imported-function-name mem bytes ilx offset)
                                )))))
