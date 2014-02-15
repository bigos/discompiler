(in-package :discompiler)

(in-suite :memory)


(test test-allocation
  (let ((mem (make-instance 'memory :start 1 :end 100)))
    (is (equalp '((100 . 100)) (allocated mem)))
    (is (equalp '((1 . 99)) (find-free mem)))
    (is (equalp 8 (allocate-preferred-block mem 3 8)))
    (is (equalp '((8 . 10) (100 . 100)) (allocated mem)))
    (is (equalp 98 (allocate-preferred-block mem 2 98)))
    (is (equalp '((8 . 10) (98 . 99) (100 . 100)) (allocated mem)))
    (is (equalp nil (allocate-preferred-block mem 3 98)))
    (is (equalp '((8 . 10) (98 . 99) (100 . 100)) (allocated mem)))
    ;; check allocated blocks
    (is (equalp 2 (length (blocks mem))))
    (is (equalp 3 (length (allocated mem))))
    ;; trying  to deallocate with incorrect block start
    (remove-allocated mem 7)
    ;; should not work
    (is (equalp 2 (length (blocks mem))))
    (is (equalp 3 (length (allocated mem))))
    ;; but deallocating giving correct block start
    (remove-allocated mem 8)
    ;; should worky
    (is (equalp '((98 . 99) (100 . 100)) (allocated mem)))
    (is (equalp 1 (length (blocks mem))))
    (is (equalp 2 (length (allocated mem))))
    ;; check allocation of first available block
    (is (equalp '((1 . 97)) (find-free mem)))
    (is (equalp '((98 . 99) (100 . 100)) (allocated mem)))
    (is (equalp  1 (allocate-available-block mem 5)))
    (is (equalp '((1 . 5) (98 . 99) (100 . 100)) (allocated mem)))
    (is (equalp '((6 . 97)) (find-free mem)))
    ;; allocation of taken preferred address gives first available
    (is (equalp 6 (allocate-block mem 9 90)))
    ;; allocate available preferred address
    (is (equalp 90 (allocate-block mem 8 90)))
    (is (equalp '((15 . 89)) (find-free mem)))))

(test test-block-addressing
  (let ((mem (make-instance 'memory :start 1 :end 100)))
    (is (equalp 1 (allocate-preferred-block mem 3 1)))
    (is (equalp 5 (allocate-preferred-block mem 3 5)))
    ;; insert data into block 0
    (is (equalp 1 (set-allocated mem 1 11)))
    (is (equalp 2 (set-allocated mem 2 12)))
    (is (equalp 3 (set-allocated mem 3 13)))
    ;; test if setting outside of block boundaries works as expected
    ;; need to work out how to check for errors in lisp-unit
    ;; and eventually use errors
    (signals simple-error (set-allocated mem 0 1))
    (signals simple-error (set-allocated mem 4 1))
    (signals simple-error (set-allocated mem 8 1))
    ;; insert data into block 1
    (is (equalp 5 (set-allocated mem 5 15)))
    (is (equalp 6 (set-allocated mem 6 26)))
    (is (equalp 7 (set-allocated mem 7 37)))
    ;; remove allocated block
    (remove-allocated mem 5)
    ;; test if setting values on deallocated block works as expected
    (signals simple-error (set-allocated mem 5 15))
    (signals simple-error (set-allocated mem 7 37))
    ;; probably have to fill newly allocated block with 0s
    (allocate-preferred-block mem 3 5)
    (signals simple-error (get-allocated mem 4))
    (is (equalp 0 (get-allocated mem 5)))
    (is (equalp 0 (get-allocated mem 6)))
    (is (equalp 0 (get-allocated mem 7)))
    (signals simple-error (get-allocated mem 8))))
