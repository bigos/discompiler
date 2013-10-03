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

(defun section-headers (bytes)
  (let ((offset)
        (optional-header-data)
        (section-data))
    (multiple-value-setq (optional-header-data offset)
      (optional-header bytes))
    (values
     (loop for x
        from 1
        to (struct-value "NumberOfSections" (coff-header bytes))
        do
          (multiple-value-setq (section-data offset)
            (section-header bytes offset))
        collect section-data)
     offset)))

(defun section-characteristics-codes ()
  '((TYPE_REG    #x00000000)
    (TYPE_DSECT  #x00000001)
    (TYPE_NOLOAD #x00000002)
    (TYPE_GROUP  #x00000004)
    (TYPE_NO_PAD #x00000008)
    (TYPE_COPY   #x00000010)
    (CNT_CODE    #x00000020)
    (CNT_INITIALIZED_DATA   #x00000040)
    (CNT_UNINITIALIZED_DATA #x00000080)
    (LNK_OTHER  #x00000100)
    (LNK_INFO   #x00000200)
    (TYPE_OVER  #x00000400)
    (LNK_REMOVE #x00000800)
    (LNK_COMDAT #x00001000)
    (NO_DEFER_SPEC_EXC #x00004000)
    (MEM_FARDATA   #x00008000)
    (MEM_PURGEABLE #x00020000)
    (MEM_16BIT     #x00020000)
    (MEM_LOCKED    #x00040000)
    (MEM_PRELOAD   #x00080000)
    (ALIGN_1BYTES  #x00100000)
    (ALIGN_2BYTES  #x00200000)
    (ALIGN_4BYTES  #x00300000)
    (ALIGN_8BYTES  #x00400000)
    (ALIGN_16BYTES #x00500000)
    (ALIGN_32BYTES #x00600000)
    (ALIGN_64BYTES #x00700000)
    (ALIGN_128BYTES #x00800000)
    (ALIGN_256BYTES #x00900000)
    (ALIGN_512BYTES #x00A00000)
    (ALIGN_1024BYTES #x00B00000)
    (ALIGN_2048BYTES #x00C00000)
    (ALIGN_4096BYTES #x00D00000)
    (ALIGN_8192BYTES #x00E00000)
    (LNK_NRELOC_OVFL #x01000000)
    (MEM_DISCARDABLE #x02000000)
    (MEM_NOT_CACHED  #x04000000)
    (MEM_NOT_PAGED   #x08000000)
    (MEM_SHARED      #x10000000)
    (MEM_EXECUTE     #x20000000)
    (MEM_READ        #x40000000)
    (MEM_WRITE       #x80000000)))

(defun section-characteristics (bytes offset)
  (let ((characteristics
         (struct-value "Characteristics" (section-header bytes offset))))
    (bitfield-flags (section-characteristics-codes) characteristics)))

(defun bitfield-flags (codes value)
  (loop for c in codes
     when
       (if (zerop value )
           (zerop (cadr c))
           (not (zerop (boole boole-and (cadr c) value ))))
     collect (car c)))

(defun section-positions (bytes)
  (let ((section-header)
        (section-start)
        (section-memory-start))
    (loop for s in (section-headers bytes)
       do
         (setf section-header (caar s))
       collect (list (struct-value "Name" s)
                     'int-on-file
                     'from
                     (setq section-start
                           (struct-value "PointerToRawData" s))
                     'to
                     (+ section-start (struct-value "SizeOfRawData" s) -1)
                     'hex-in-memory
                     (int-to-hex (setq section-memory-start
                                       (+ (image-base bytes)
                                          (struct-value "VirtualAddress" s))))
                     'to
                     (int-to-hex
                      (+ section-memory-start
                         (struct-value "VirtualSize" s) -1))
                     #|'or
                     (int-to-hex
                      (+ section-memory-start
                         (struct-value "SizeOfRawData" s) -1))|#
                     (bitfield-flags
                      (section-characteristics-codes)
                      (struct-value "Characteristics" s))))))


(defun image-base (bytes)
  (struct-value "ImageBase"(optional-header bytes)))

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
    ;;
    (format t "~&PE header signature is ~a~%~%"
            (if (pe-header-signature-validp bytes) "valid" "INVALID"))
    (format t "~&loaded image base addr ~%int ~S~%hex ~S ~%~%"
            (struct-value "ImageBase" opt-head)
            (int-to-hex (struct-value "ImageBase" opt-head)))
    (format t "~& entry point in memory ~S~%~%"
            (int-to-hex (+ (image-base bytes)
                           (struct-value "AddressOfEntryPoint" opt-head))))
    (format t "RVAs: ~S~%~%" used-rvas)
    (format t "sections ~a~%~%" (section-positions bytes))
    my-sections))
