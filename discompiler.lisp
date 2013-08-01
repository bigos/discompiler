(in-package :discompiler)

(defun run ()
  (format t "running skeleton program")
  (test-arithmetic)
  (test-mod-vals)
  (run-tests :all))

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

(defun byte-parts (hex)
  (let ((bin (hex-to-bin hex))a)
    (list (bin-to-int (subseq bin 0 2))
          (bin-to-int (subseq bin 2 5))
          (bin-to-int (subseq bin 5 8)))))

(defun modrmreg-vals (hex)
  (let ((parts (byte-parts hex)))
    (list (first parts) (third parts) (second parts))))

(defun mod-part (hex)
  (bin-to-int (subseq (hex-to-bin hex) 0 2)))
(defun rm-part (hex)
  (bin-to-int (subseq (hex-to-bin hex) 5 8)))
(defun reg-part (hex)
  (bin-to-int (subseq (hex-to-bin hex) 2 5)))

(defun ss-part (hex)
  (bin-to-int (subseq (hex-to-bin hex) 0 2)))
(defun index-part (hex)
  (bin-to-int (subseq (hex-to-bin hex) 2 5)))
(defun base-part (hex)
  (bin-to-int (subseq (hex-to-bin hex) 5 8)))

