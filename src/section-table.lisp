(in-package :discompiler)

(defun section-header (bytes offset)
  (let ((elements '((+double-long+ "Name")
                    (+long+ "VirtualSize")
                    (+long+ "VirtualAddress")
                    (+long+ "SizeOfRawData")
                    (+long+ "PointerToRawData")
                    (+long+ "PointerToRelocations")
                    (+long+ "PointerToLinenumbers")
                    (+short+ "NumberOfRelocations")
                    (+short+ "NumberOfLinenumbers")
                    (+long+ "Characteristics"))))
    (multiple-value-bind (data structure-size)
        (c-structure-values bytes elements offset)
      (values
       (cons (append (butlast (car data))
                     (list (int-to-text (car (last (car data))))))
             (cdr data))
       structure-size))))

(defun sections (bytes)
  (let ((optional-header-data)
        (section-data)
        (offset))
    (multiple-value-setq (optional-header-data offset)
      (optional-header bytes))
    (cdr
     (loop for x
        to (struct-value "NumberOfSections" (coff-header bytes))
        collecting section-data
        do
          (multiple-value-setq (section-data offset)
            (section-header bytes offset))))))
