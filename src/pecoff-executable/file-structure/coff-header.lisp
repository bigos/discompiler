(in-package :discompiler)

(defun coff-header-pointer (bytes)
  (+ (pe-header-signature-pointer bytes) 4))

(defun coff-header (bytes)
  (let ((offset (coff-header-pointer bytes))
        (elements '((+short+ "Machine")
                    (+short+ "NumberOfSections") ; I need to find why coff header in one file is different when I use debugger
                    (+long+ "TimeDateStamp")
                    (+long+ "PointerToSymbolTable")
                    (+long+ "NumberOfSymbols")
                    (+short+ "SizeOfOptionalHeader")
                    (+short+ "Characteristics"))))
    (multiple-value-bind (data structure-size)
        (c-structure-values bytes elements offset)
      (values data structure-size))))

(defun coff-characteristics (bytes)
  (let ((characteristics (struct-value "Characteristics" (coff-header bytes)))
        (codes '(RELOCS_STRIPPED
                 EXECUTABLE_IMAGE
                 LINE_NUMS_STRIPPED
                 LOCAL_SYMS_STRIPPED
                 AGGRESSIVE_WS_TRIM
                 LARGE_ADDRESS_AWARE
                 16BIT_MACHINE
                 BYTES_REVERSED_LO
                 32BIT_MACHINE
                 DEBUG_STRIPPED
                 REMOVABLE_RUN_FROM_SWAP
                 ()
                 SYSTEM
                 DLL
                 UP_SYSTEM_ONLY
                 BYTES_REVERSED_HI)))
    (flag-names codes characteristics)))

(defun coff-value (name bytes)
  (struct-value name (coff-header bytes)))
