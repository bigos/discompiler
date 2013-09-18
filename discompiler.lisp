(in-package :discompiler)

(defun run ()
  (format t "running skeleton program")
  ;; (test-arithmetic)
  ;; (test-mod-vals)
  ;; (run-tests :all)
  (defparameter *problem-files* '())
  ;; (car (nth 472 *instructions*))
  ;; ("XTEST — Test If In Transactional Execution" "Opcode/Instruction Op/En"
  ;; "64/32bit" "Mode" "Support" "CPUID" "Feature" "Flag" "Description")
  ;; wow!!!
  (defparameter *instructions* (reference-data:process-reference-files))
  (list-length *instructions*))

(defparameter *dll* (nth 0 (cl-fad:list-directory "./Test executables/"))
  "sample dll file")

;; size of datatyoe in bytes
(defparameter *char-size* 1)
(defparameter *short-size* 2)
(defparameter *long-size* 4)


(defun file-to-bytes (file)
  (let ((bytes) (index 0) (size))
    (with-open-file (stream file :element-type 'unsigned-byte)
      (setf size (file-length stream))
      (setf bytes (make-array size))
      (do ((byte (read-byte stream nil)
                 (read-byte stream nil)))
          ((null byte))
        (setf (aref bytes index) byte)
        (incf index))
      (values bytes size))))

(defun pe-header-signature-pointer (bytes)
  (bytes-to-type-int (bytes bytes +long+ 60)))

(defun pe-header-signature-validp (bytes)
  (let ((pointer (pe-header-signature-pointer bytes)))
    (if (array-in-bounds-p bytes pointer)
        (equalp
         (bytes bytes +long+ pointer)
         '(80 69 0 0))
        nil)))

(defun coff-header-pointer (bytes)
  (+  (pe-header-signature-pointer bytes) 4))

(defun struct-value (name struct)
  (dolist (el struct)
    (if (equalp (nth 1 el) name) (return (nth 2 el)))))

(defun c-structure-values (bytes c-structure offset)
  (values
   (loop for el in c-structure
      collecting (list offset
                       (cadr el)
                       (bytes-to-type-int
                        (bytes bytes (eval (car el)) offset)))
      do
        (incf offset (eval (car el))))
   offset))

(defun msdos-header (bytes)
  (let ((offset 0)
        (structure-size)
        (data)
        (elements '(((* 2 +char+) "signature")
                    (+short+ "lastsize")
                    (+short+ "nblocks")
                    (+short+ "nreloc")
                    (+short+ "hdrsize")
                    (+short+ "minalloc")
                    (+short+ "maxalloc")
                    (+void+ "*ss")
                    (+void+ "*sp")
                    (+short+ "checksum")
                    (+void+ "*ip")
                    (+void+ "*cs")
                    (+short+ "relocpos")
                    (+short+ "noverlay")
                    ((* 4 +short+) "reserved1")
                    (+short+ "oem_id")
                    (+short+ "oem_info")
                    ((* 10 +short+) "reserved2")
                    (+long+ "e_lfanew"))))
    (multiple-value-setq (data structure-size) 
      (c-structure-values bytes elements offset))
    (values data structure-size)))

(defun coff-header (bytes)
  (let ((offset (coff-header-pointer bytes))
        (structure-size)
        (data)
        (elements '((+short+ "Machine")
                    (+short+ "NumberOfSections")
                    (+long+ "TimeDateStamp")
                    (+long+ "PointerToSymbolTable")
                    (+long+ "NumberOfSymbols")
                    (+short+ "SizeOfOptionalHeader")
                    (+short+ "Characteristics"))))
    (multiple-value-setq (data structure-size)
      (c-structure-values bytes elements offset))
    (values data structure-size)))

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

(defun flag-names (flags value)
  (loop for bit from 0 to (1- (list-length flags))
     for flag in flags
     when (and 
           (not (zerop (ldb (byte 1 bit) value)))
           flag)
     collect flag))

(defun coff-value (name bytes)
  (struct-value name (coff-header bytes)))

(defun byte-at (bytes offset)
  (aref bytes offset))

(defun bytes (bytes count offset)
  "get number of bytes for C structure datatypes"
  (loop for x to (1- count)
     collecting (byte-at bytes (+ offset x))))

(defun bytes-to-type-int (bytelist)     ;ignoring endianness
  (loop for x to (1- (list-length bytelist))
     summing (* (nth x bytelist) (expt 2 (* 8 x)))))

(defun instruction-volume-page (mnemonic)
  (let ((prev 0) (current) (vol "a"))
    (dolist (inst *instructions*)
      (setq current (parse-integer(caar inst)))
      (unless (> current prev)
        (setq vol "b"))
      ;;(format t "~& ~S ~S" (instruction-mnemonics-string inst) (cons vol current))
      (setq prev current)
      (if (find mnemonic (instruction-mnemonics-list inst) :test #'equalp)
          (return (cons vol current))))))

(defun pdf-documentation-page (mnemonic)
  ;; run like this: (pdf-documentation-page "aaa")
  ;; remember to change vola volb and pdf-viewer on a different machine
  (let* ((vola "/home/jacek/Documents/Manuals/IntelDocumentation/latest/253666.pdf")
         (volb "/home/jacek/Documents/Manuals/IntelDocumentation/latest/253667.pdf")
         (pdf-viewer "/usr/bin/atril")
         (volpa (instruction-volume-page mnemonic))
         (volume (car volpa)) (page (cdr volpa)))
    (sb-ext:run-program pdf-viewer
                        `("-p"
                          ,(format nil "~d" page)
                          ,(if (equalp volume "a") vola volb))
                        :wait nil)))

(defun instruction-title (instruction)
  (subseq (car instruction)) 1)

(defun instruction-columns (instruction)
  (nth 1 instruction))

(defun column-keywords (instruction)
  (let ((column-data (instruction-columns instruction)))
    (sort (map 'list
               #'(lambda (x) (string-trim "/" x))
               (remove-if 'emptystrp
                          (cl-utilities:split-sequence #\space
                                                       (list-to-string
                                                        column-data))))
          #'string-lessp)))


(defun emptystrp (string)
  (if (equal string "")
      T
      nil))

(defun list-to-string (my-list &optional (separator " "))
  (let ((result))
    (dolist (item my-list)
      (setf result (concatenate 'string
                                result
                                (format nil "~A~a" item separator))))
    result))

(defun show-suspected ()
  (let ((ci))
    (dolist (inst *instructions*)
      (setf ci (cadr inst))
      (if (> (list-length ci) 1)
          (format t "~&~a ~%~%~%~s ~%" (length  ci)  ci )))))

(defun all-mnemonics ()
  (sort
   (flatten
    (loop
       for inst in *instructions*
       collecting (cl-utilities:split-sequence
                   #\/
                   (instruction-mnemonics-string inst))))
   #'string-lessp))

(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun mnemonic-columns (mnemonics)
  (dolist (d (summary-table-column-data))
    (if (search `(,mnemonics) (nth 1 d) :test #'equalp)
        (return (nth 0 d)))))

(defun instruction-mnemonics-string (instruction)
  (let ((separator "—")
        (title (cadar instruction)))
    (string-trim " " (subseq title 0 (search  separator title)))))

(defun instruction-mnemonics-list (instruction)
  (cl-utilities:split-sequence #\/ (instruction-mnemonics-string instruction)))

(defun uniques (instructions)
  (let ((columns) (mnemonics) (found) (column-mnemonics))
    (dolist (i instructions)
      (setf columns (column-keywords i))
      (setf mnemonics (instruction-mnemonics-string i))
      (setf found nil)
      ;;(format t "~s ~s ~S~%" found columns mnemonics)
      (loop for cm in column-mnemonics
         when (equalp columns (car cm))
         do
           (progn
             (push mnemonics (cadr cm))
             ;;(format t ">>>>> ~S ~S~%" cm (cadr cm))
             (setf found T)
             (loop-finish)))
      (unless found
        (progn
          ;;(format t "not found ~%")
          (push `(,columns (,mnemonics)) column-mnemonics))))
    column-mnemonics))
