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
    (format t "RVAs: ~S~%~%" used-rvas)
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

;; http://msdn.microsoft.com/en-us/magazine/cc301805.aspx
;; the linker often merges parts of the .idata into other sections,
;; such as .rdata, when doing a release build.


;; another find
;; .rdata secion on file has two sequences starting with F0 20
;; one starts at a00 and the other starts at a60
;; perhaps the first sequence gets overwritten with IAT RVA data by the loader


;; guess

;; hint table on file at A84
;; two import lookup tables start at A60 and A70
;; file offsets A30 and A44 contain names of libraries

;; for more infoo look ar pecoff_v83.pdf .idata section on pag 106

;; beginning of import table starting on file at A24 points to second
;; import lookup table A70, RVA value points to 402070 and it is the first table
;; element from page 107

;; TODO - Perhaps I need to rewrite loading of executable. Looks like I have to
;; copy it to "memory" before before I start dealing with RVAs

(defun hex-rva-addr (rva bytes)
  "hex address of RVA in memeory"
  (int-to-hex (+ rva (image-base bytes))))
