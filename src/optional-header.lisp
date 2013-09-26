(in-package :discompiler)

(defun optional-header-signature-pointer (bytes)
  (multiple-value-bind (d s) (coff-header bytes)
    s))

(defun optional-header-signature (bytes)
  (bytes-to-type-int
   (bytes bytes +short+ (optional-header-signature-pointer bytes))))

(defun optional-header-image-type (bytes)
  (let ((signature (optional-header-signature bytes)))
    (cond ((eq signature #x10b) 'pe32)
          ((eq signature #x20b) 'pe32+)
          (T nil))))

(defun optional-header (bytes)
  (let* ((offset (optional-header-signature-pointer bytes))
         (header-type (optional-header-image-type bytes))
         (long-or-double (if (eq (optional-header-image-type bytes) 'PE32)
                             '+long+
                             '+double-long+))
         (elements (append '((+short+ "Magic")
                             (+char+ "MajorLinkerVersion")
                             (+char+ "MinorLinkerVersion")
                             (+long+ "SizeOfCode")
                             (+long+ "SizeOfInitializedData")
                             (+long+ "SizeOfUninitializedData")
                             (+long+ "AddressOfEntryPoint")
                             (+long+ "BaseOfCode"))
                           (when  (eq header-type 'PE32)
                             '((+long+ "BaseOfData")))
                           `((,long-or-double "ImageBase")
                             (+long+ "SectionAlignment")
                             (+long+ "FileAlignment")
                             (+short+ "MajorOperatingSystemVersion")
                             (+short+ "MinorOperatingSystemVersion")
                             (+short+ "MajorImageVersion")
                             (+short+ "MinorImageVersion")
                             (+short+ "MajorSubsystemVersion")
                             (+short+ "MinorSubsystemVersion")
                             (+long+ "Reserved")
                             (+long+ "SizeOfImage")
                             (+long+ "SizeOfHeaders")
                             (+long+ "CheckSum")
                             (+short+ "Subsystem")
                             (+short+ "DLLCharacteristics")
                             (,long-or-double "SizeOfStackReserve")
                             (,long-or-double "SizeOfStackCommit")
                             (,long-or-double "SizeOfHeapReserve")
                             (,long-or-double "SizeOfHeapCommit")
                             (+long+ "LoaderFlags")
                             (+long+ "NumberOfRvaAndSizes"))
                           '((+long+ "Export Table RVA")
                             (+long+ "Export Table Size")
                             (+long+ "Import Table RVA")
                             (+long+ "Import Table Size")
                             (+long+ "Resource Table RVA")
                             (+long+ "Resource Table Size")
                             (+long+ "Exception Table RVA")
                             (+long+ "Exception Table Size")
                             (+long+ "Certificate Table RVA")
                             (+long+ "Certificate Table Size")
                             (+long+ "Base Relocation Table RVA")
                             (+long+ "Base Relocation Table Size")
                             (+long+ "Debug RVA")
                             (+long+ "Debug Size")
                             (+long+ "Architecture RVA")
                             (+long+ "Architecture Size")
                             (+long+ "Global Ptr RVA")
                             (+long+ "Global Ptr Size")
                             (+long+ "TLS Table RVA")
                             (+long+ "TLS Table Size")
                             (+long+ "Load Config Table RVA")
                             (+long+ "Load Config Table Size")
                             (+long+ "Bound Import RVA")
                             (+long+ "Bound Import Size")
                             (+long+ "IAT RVA")
                             (+long+ "IAT Size")
                             (+long+ "Delay Import Descriptor RVA")
                             (+long+ "Delay Import Descriptor Size")
                             (+long+ "COM+ Runtime Header RVA")
                             (+long+ "COM+ Runtime Header Size")
                             (+long+ "Reserved RVA")
                             (+long+ "Reserved Size")))))
    (multiple-value-bind (data structure-size)
        (c-structure-values bytes elements offset)
      (values data structure-size))))
