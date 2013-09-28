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
  (let ((number-of-sections
         (struct-value "NumberOfSections" (coff-header bytes)))
        (data) 
        (section-data) 
        (offset))
    (multiple-value-setq (data offset) (optional-header bytes))
    (cdr
     (loop
        for secn from 0 to number-of-sections
        collecting (list (nth 0 (car section-data)) 
                         (nth 2 (car section-data)))
        do
          (multiple-value-setq (section-data offset)
            (section-header bytes offset))))))
