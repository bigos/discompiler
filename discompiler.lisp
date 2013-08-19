(in-package :discompiler)

(defun run ()
  (format t "running skeleton program")
  ;; (test-arithmetic)
  ;; (test-mod-vals)
  ;; (run-tests :all)
  (defparameter *problem-files* '())
  (defparameter *reference-files* (cl-fad:list-directory "my-reference"))
  ;;(process-file (car *reference-files*))
  (dolist (file (subseq *reference-files* 0 2)) ;remove subseq to check every file
    (process-file file))
  (format t "~&no separator in following files ~S~%" *problem-files*))

(defun process-file (file)
  (let ((xl 0) (lines))
    (with-open-file (stream file) 
      (format t "~&~a ~s~%" (file-length stream) file)
      (setf lines (make-array (file-length stream) :element-type 'string))
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        ;;(format t "~&~A" line)
        (process line)
        (if (zerop xl)
            (if (equalp line "")
                (push file *problem-files*)))
        (setf (aref lines xl) line)
        (incf xl)))
    (check-lines file lines xl)))

(defun file-to-lines (file)
  (let ((lines))
    (with-open-file (stream file)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (setq lines (nconc lines (list line)))))
    lines))  

(defun check-lines (file lines xl)
  (let ((separators 0))
    (dotimes (x xl) 
      (if (separatorp (aref lines x))
          (incf separators)))
    (if (zerop separators)
        (push file *problem-files*))
    ))

(defun process (line)
  (cond ((blankp line) 
         (print line))
        ((separatorp line)
         (format t "~&~s ~D~%" line (length line)))
        (t (format t "."))))

(defun blankp (line)
  (cl-ppcre:scan-to-strings "\\A\\z" line))

(defun separatorp (line)
  (cl-ppcre:scan-to-strings "-{10,}" line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun int-to-bin (int) 
  (format nil "~8,'0B" int))

(defun int-to-hex (int)
  (format nil "~x" int))

(defun hex-to-int (hex)
  (parse-integer hex :radix 16))

(defun hex-to-bin (hex)
  (int-to-bin (hex-to-int hex)))

(defun bin-to-int (bin)
  (parse-integer bin :radix 2))

(defun byte-part (bin start end)
  (bin-to-int (subseq bin start end)))

(defun byte-parts (hex)
  (let ((bin (hex-to-bin hex))a)
    (list (byte-part bin 0 2)
          (byte-part bin 2 5)
          (byte-part bin 5 8))))

(defun modrmreg-vals (hex)
  (let ((parts (byte-parts hex)))
    (list (first parts) (third parts) (second parts))))

(defun mod-part (hex)
  (byte-part (hex-to-bin hex) 0 2))
(defun rm-part (hex)
  (byte-part (hex-to-bin hex) 5 8))
(defun reg-part (hex)
  (byte-part (hex-to-bin hex) 2 5))

(defun ss-part (hex)
  (byte-part (hex-to-bin hex) 0 2))
(defun index-part (hex)
  (byte-part  (hex-to-bin hex) 2 5))
(defun base-part (hex)
  (byte-part  (hex-to-bin hex) 5 8))

(defun get-reg (reg)
  "general-purpose registers, ia-32 vol1 p60"
  (let ((registers '(('eax ('ax ('ah 'al)))
                     ('ebx ('bx ('bh 'bl)))
                     ('ecx ('cx ('ch 'cl)))
                     ('edx ('dx ('dh 'dl)))
                     ('ebp ('bp))
                     ('esi ('si))
                     ('edi ('di))
                     ('esp ('sp)))))
    nil
    ))

(defun get-segment (reg)
  "segment registers ia32 vol1 p61"
  (let ((segments '('cs 'ds 'ss 'es 'fs 'gs)))
    nil
    ))

(defun intel-bit-position (index &optional (width 8)) 
  (if (typep index 'integer) 
      (cond ((>= index width) (error "index ~a should not be >= ~a" index width))
            ((< index 0) (error "index should not be less than 0"))
            (t (- (1- width) index)))
      (error "expected integer but ~a given" index))) 
