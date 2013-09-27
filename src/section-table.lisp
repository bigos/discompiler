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
       (list  (int-to-text (car (last (car data)))) (cdr data)) 
       structure-size))))

