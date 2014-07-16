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
  (let ((libraries-path "~/discompiler/SampleExecutables/PE/DLLs/")
        (library-name (get-allocated-string mem (rva-addr-in-struct "NameRVA"
                                                                    directory-table
                                                                    bytes)))
        (found nil))
    (mapc-directory-tree (lambda (x)
                           ;; check if needed library exists on disk
                           (when (equalp library-name (full-filename x))
                             (setf found library-name)))
                         libraries-path)
    ;; found shold be known now
    (format t "found ??????? ~A~%" found)
    (mapc-directory-tree (lambda (x)
                           (when (equalp library-name (full-filename x))
                             (format t "library on disk >>>>>>>>>>: ~S wanted: ~S result: ~S~%"
                                     (full-filename x)
                                     library-name
                                     (equalp library-name (full-filename x)))
                             ;; (format t "~%~%modules:  ~S~%" (modules mem))
                             (if (loaded? x mem)
                                 ;; (format t "going to load ~S~%" x)
                                 ;; actually load the file bytes and pass them to loader-1
                                 ;; (format t "going to try ~A~%" x)
                                 (loader-1 x mem (file-to-bytes x))
                                 (format t "+++++ not found library ~A ~%" x))))
                         libraries-path)
    library-name))

(defun loaded? (file mem)
  (proclaim '(optimize (speed 0) (space 0) (debug 3))) ;
  (loop for m in (modules mem)
     for found = (if (equalp (full-filename (module-fulldllname m))
                             (full-filename file))
                     (full-filename file)
                     nil)
     until found
     ;; do
     ;;   (cerror "loaded loop" "check it")
     finally (progn
               (format t ">>> found ~A ~A ~A~%" found file (module-fulldllname m))
               (return found))))

(defun import-by-ordinalp (bytes ilx)
  (= 1 (ldb (byte 1 (if (eq 'PE32 (optional-header-image-type bytes))
                        31
                        63))
            ilx)))

(defun imported-function-hint (mem bytes ilx)
  (bytes-to-type-int
   (get-allocated-bytes mem
                        (rva-addr ilx bytes) 2)))

(defun imported-function-name (mem bytes ilx)
  (get-allocated-string mem
                        (rva-addr (+ 2 ilx)
                                  bytes)))

(defun imported-ordinal-name (ilx)
  ;;TODO find more efficient way
  (let ((ordinal-names (ordinal-names
                        (file-export-list
                         "./SampleExecutables/PE/ordinal-imports.dll")))
        (ordinal-number (ldb (byte 16 0) ilx) ))
    (cons ordinal-number
          (loop for x from 0 below (length ordinal-names)
             for ex = (aref ordinal-names x)
             until (eq (car ex) ordinal-number)
             ;;do (format t "~s~%" ex)
             finally (return (cdr ex))))))

(defun imported-function-names (mem bytes imp-dir-rva)
  (loop for il from imp-dir-rva by 4
     for ilx = (bytes-to-type-int (get-allocated-bytes mem (rva-addr il bytes) 4))
     until (zerop ilx)
     collect (list il
                   ilx
                   (if (import-by-ordinalp bytes ilx)
                       (imported-ordinal-name ilx)
                       (cons (imported-function-hint mem bytes ilx)
                             (imported-function-name mem bytes ilx))))))

(defun imported-functions (bytes mem)
  (declare (optimize (debug 3) (safety 3)))
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
         do
           (push (library-name mem bytes idt) *required*)
         collect
           (list
            (library-name mem bytes idt)
            (imported-function-names mem bytes imp-dir-rva))
           ))))
