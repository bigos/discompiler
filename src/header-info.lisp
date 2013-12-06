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
                       collect
                         (list
                          (nth x rvas)
                          (nth (1+ x) rvas)
                          "in memory from"
                          (int-to-hex
                           (+ (struct-value "ImageBase" opt-head)
                              (nth 2 (nth x rvas))))
                          "to"
                          (int-to-hex
                           (+ (struct-value "ImageBase" opt-head)
                              (nth 2 (nth x rvas))
                              (nth 2 (nth (1+ x) rvas))
                              ))
                          )
                         ))
         (my-sections (section-headers bytes)))
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
    (format t "RVAs: ~S~%~%" (sort used-rvas
                                   #'<
                                   :key #'(lambda (x) (nth 2 (car x)) )))
    (format t "last 4 byte element of optional header hex ~X sections headers follow~%~%" (caar (last (optional-header *bytes*))))
    (format t "sections ~a~%~%" (section-positions bytes))
    my-sections)
  )

;; pecoff page 90
;; However, some COFF sections have special meanings when found
;; in object files or image files. Tools and loaders recognize these
;; sections because they have special flags set in the section header,
;; because special locations in the image optional header point to
;; them, or because the section name itself indicates a special
;; function of the section.


;; http://msdn.microsoft.com/en-us/magazine/cc301805.aspx
;; section Importing Functions
;; implicit vs explicit linking of libraries

;; http://msdn.microsoft.com/en-us/magazine/cc301808.aspx
;; imports section might help to move forward
