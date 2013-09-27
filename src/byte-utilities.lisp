(in-package :discompiler)

(defun int-to-bin (int)
  (format nil "~8,'0B" int))

(defun int-to-hex (int)
  (format nil "~x" int))


(defun int-to-text (int)
  (let ((hexval  (int-to-hex int)))
    (reverse
     (loop 
        for start from 0 to (- (length hexval) 2) by 2
        for end from 2 to (length hexval) by 2
        collecting
          (code-char (hex-to-int (subseq hexval start end)))))))

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
