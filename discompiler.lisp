(in-package :discompiler)

(defun project-path ()
    (if (equalp (machine-instance) "jaceks-imac.chess")
        "/Users/jacekpodkanski/Documents/SBCL/"
        "/home/jacek/Programming/Lisp/"))

(defun all-symbols-defined (package)
  (let ((pack (find-package package))
        (res))
    (do-all-symbols (sym pack)
      (when (eql (symbol-package sym) pack) (push sym res)))
    (reverse res)))

(defun run-prog ()
  (format t "running skeleton program")
  ;; (test-arithmetic)
  ;; (test-mod-vals)
  ;; (run-tests :all)
  (defparameter *problem-files* '())
  ;; (car (nth 472 *instructions*))
  ;; ("XTEST — Test If In Transactional Execution" "Opcode/Instruction Op/En"
  ;; "64/32bit" "Mode" "Support" "CPUID" "Feature" "Flag" "Description")
  ;; wow!!!
  (defparameter *instructions* (reference-data::process-reference-files))
  (list-length *instructions*))

(defparameter *dll* (nth 0 (cl-fad:list-directory "./SampleExecutables/"))
  "sample dll file")

(defun mapc-directory-tree (fn directory)
  (dolist (entry (cl-fad:list-directory directory))
    (when (cl-fad:directory-pathname-p entry)
      (mapc-directory-tree fn entry))
    (funcall fn entry)))

(defun file-to-bytes (file)
  "Return FILE contents as a vector of unsigned bytes."
  (alexandria:read-file-into-byte-vector file))

(defun load-bytes (file)
  "use this function to avoid showing massive output for large files"
  (setf *bytes* (file-to-bytes file))
  "loaded")

(defparameter *bytes* (file-to-bytes "~/discompiler/SampleExecutables/PE/crackme12.exe"))

(defun struct-value (name struct)
  (dolist (el struct)
    (when (equalp (nth 1 el) name) (return (nth 2 el)))))

(defun c-structure-values (bytes c-structure initial-offset)
  (loop
     for el in c-structure
     for value-size = (if (symbolp (car el))
                          (symbol-value (car el))
                          (car el))
     and offset = initial-offset then (+ offset value-size)
     collecting (list offset
                      (cadr el)
                      (bytes-to-type-int (bytes bytes value-size offset)))
     into collected
     finally (return (values collected (+ value-size offset)))))


(defun flag-names (flags value)
  (loop for bit from 0 to (1- (list-length flags))
     for flag in flags
     when (and
           (not (zerop (ldb (byte 1 bit) value)))
           flag)
     collect flag))

(defun aligned-size (size alignment)
  (* alignment (1+ (floor (1- size) alignment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun byte-at (bytes offset)
  (aref bytes offset))

(defun bytes (bytes count offset)
  "get number of bytes for C structure datatypes"
  (loop for x to (1- count)
     collecting (byte-at bytes (+ offset x))))

(defun bytes-hex (bytes count offset &optional (columns 16))
  (loop for x below count
   do
     (when (zerop (mod x columns)) (format t "~&~16,8R: " (+ offset x)))
     (format t "~16,2R " (byte-at bytes (+ offset x)))
     ))


(defun bytes-to-type-int (bytelist)     ;ignoring endianness
  (loop for x below (length bytelist)
     summing (*
              (if (equalp (type-of bytelist) 'cons)
                  (nth x bytelist)
                  (aref bytelist x))
              (expt 2 (* 8 x)))))

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
  (equal string ""))

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
       collecting (cl-utilities:split-sequence []
                                               #\/
                                               (instruction-mnemonics-string inst))))
   #'string-lessp))

(defun flatten (structure)
  ;;TODO doesn't work with '((1 . :a) (2 . :b))
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
  (loop for i in instructions
     for columns = (column-keywords i)
     for mnemonics = (instruction-mnemonics-string i)
     for found = nil
     do ;;(format t "~s ~s ~S~%" found columns mnemonics)
       (loop for cm in column-mnemonics
          when (equalp columns (car cm))
          do
            (push mnemonics (cadr cm)) ;(format t ">>>>> ~S ~S~%" cm (cadr cm))
            (setf found T)
            (loop-finish))
     unless found
     ;;(format t "not found ~%")
     collect `(,columns (,mnemonics))))
