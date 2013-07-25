(in-package :discompiler)

(defun run ()
  (format t "running skeleton program")
  (test-arithmetic)
  (test-mod-vals))

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

(defun modrmreg-vals (hex)
  (let* ((bin (hex-to-bin hex))
         (-mod (subseq bin 0 2))
         (reg (subseq bin 2 5))
         (rm (subseq bin 5 8)))
    (list (bin-to-int -mod) (bin-to-int rm) (bin-to-int reg))))

(defun sib-vals (hex)
  (let* ((bin (hex-to-bin hex))
         (ss (subseq bin 0 2)) 
         (index(subseq bin 2 5)) 
         (base(subseq bin 5 8)))
    (list (bin-to-int ss) (bin-to-int index) (bin-to-int base))))
