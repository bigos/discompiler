(in-package :discompiler)

(defun image-base (bytes)
  (struct-value "ImageBase"(optional-header bytes)))

(defun cons-int-hex (val)
  (cons  val (concatenate 'string "#x" (int-to-hex val))))

(defun useful-info (bytes)
  (let* ((opt-head (optional-header bytes))
         (entry-point (struct-value "AddressOfEntryPoint" opt-head))
         (code-base (struct-value "BaseOfCode" opt-head))
         (data-base (struct-value "BaseOfData" opt-head))
         (header-type (optional-header-image-type bytes))
         (first-rva 30)
         (last-rva (+ 30 32))
         (rvas (subseq opt-head first-rva last-rva))
         (used-rvas (loop for x
                       from 0
                       to (1- (list-length rvas))
                       by 2
                       when (not (zerop (caddr (nth x rvas))))
                       collect (list (nth x rvas) (nth (1+ x) rvas))))
         (my-sections (section-headers bytes)))
    ;; (format t "~%~%>>>~S~%~%~%" rvas)
    (format t "~&PE header signature is ~a~%~%"
            (if (pe-header-signature-validp bytes) "valid" "INVALID"))
    (format t "header type ~a~%" header-type)
    (format t "section alignment~S~%~%"
            (cons-int-hex (struct-value "SectionAlignment" opt-head)))
    (if (< (struct-value "SectionAlignment" opt-head) 4096)
        (princ "warning section alignment less than 4K - constraints on the file offset of the section data,"))
    (format t "~&loaded image base addr ~S~%"
            (cons-int-hex (struct-value "ImageBase" opt-head)))
    (format t "~& entry point in memory #x~a~%~%"
            (int-to-hex (+ (image-base bytes)
                           (struct-value "AddressOfEntryPoint" opt-head))))
    (format t "RVAs: ~S~%~%" used-rvas)
    (format t "sections ~a~%~%" (section-positions bytes))
    my-sections))
