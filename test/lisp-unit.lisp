(in-package :discompiler)

(setf *print-failures* t)


(define-test test-sample-file
  "sample executable file"
  (let* ((file "~/discompiler/SampleExecutables/crackme12.exe")
         (bytes (file-to-bytes file)))
    (assert-equalp #(17 122 62 7 172 101 207 43 236 55 231 193 95 182 209 19)
                   (md5:md5sum-file file))
    (assert-equal (pe-header-signature-pointer bytes)
                  192)
    (assert-eq (pe-header-signature-validp bytes)
               T)
    (assert-equalp (coff-characteristics bytes)
                   '(RELOCS_STRIPPED
                     EXECUTABLE_IMAGE
                     LINE_NUMS_STRIPPED
                     LOCAL_SYMS_STRIPPED
                     32BIT_MACHINE))
    (assert-eq (optional-header-signature bytes)
               #x10b)
    (assert-eq (optional-header-image-type bytes)
               'PE32)))

(define-test test-allocation
  (let ((start 3)
        (end 20)
        (allocated '((5 . 7) (12 . 15)))
        (allocated-edges '((3 . 8) (16 . 20))))
    (assert-equalp '((3 . 4) (8 . 11) (16 . 20))
                   (find-free allocated start end))
    (assert-equalp '((9 . 15))
                   (find-free allocated-edges start end))
    (assert-equalp 3 (find-free-block allocated start end 2 ))
    (assert-equalp 8 (find-free-block allocated start end 4 ))
    (assert-equalp 16 (find-free-block allocated start end 5 ))
    (assert-equalp nil (find-free-block allocated start end 6))
    (assert-equalp 9 (find-free-block allocated-edges start end 7))
    (assert-equalp nil (find-free-block allocated-edges start end 8))
    (assert-equal T (is-block-available allocated start 2))
    (assert-equal nil (is-block-available allocated start 3))))

(define-test test-addition
  "test simple addition"
  (assert-equal 3 (+ 1 2))
  (assert-equal 1 (+ 0 1))
  (assert-equal 17 (1- (* 3 6)))
  (assert-equal 6 (/ 6 1))
  (assert-equal 6 (+ 1 2 3)))

(define-test test-hex-bin
  "converting hex to binary"
  (assert-equal "00000000" (int-to-bin 0))
  (assert-equal "11111111" (int-to-bin 255))
  (assert-equal "10101010" (int-to-bin 170)))

(define-test mod-rm
    "testing correct values of modrm and sib byte fiels"
  (assert-equal 1 (mod-part "6b"))
  (assert-equal 3 (rm-part "6b"))
  (assert-equal 5 (reg-part "6b"))

  (assert-equal 1 (ss-part "5d"))
  (assert-equal 3 (index-part "5d"))
  (assert-equal 5 (base-part "5d"))
  (assert-equal 2 (ss-part "a6"))
  (assert-equal 4 (index-part "a6"))
  (assert-equal 6 (base-part "a6")))

(define-test intel-bit-position
  "in Intel documentation bit 0 is the last one and bit 7 is the first one"
  (assert-equal 0 (intel-bit-position 7))
  (assert-equal 1 (intel-bit-position 6))
  (assert-equal 2 (intel-bit-position 5))
  (assert-equal 3 (intel-bit-position 4))
  (assert-equal 4 (intel-bit-position 3))
  (assert-equal 5 (intel-bit-position 2))
  (assert-equal 6 (intel-bit-position 1))
  (assert-equal 7 (intel-bit-position 0))
  ;;(assert-equal 1 "00000001")
  ;;(assert-equal 1 "10000000")
  )

;; (define-test instructions
;;     "instruction reference"
;;   ;; is it the best way to do it?
;;   ;; what about VEX?
;;   (37) (AAA) (ASCII adjust AL after addition)

;;   (D5 0A) (AAD) (ASCII adjust AX before division)
;;   (D5 ib) (No-mnemonic) (Adjust AX before division to number base imm8)


;;   (D4 0A) (AAM) (ASCII adjust AX after multiply)
;;   (D4 ib) (No-mnemonic) (Adjust AX after multiply to number base imm8)

;;   (3F) (AAS) (ASCII adjust AL after subtraction)

;;   (14 ib) (ADC AL,imm8) (Add with carry imm8 to AL)
;;   15 iw ADC AX,imm16 Add with carry imm16 to AX
;;   15 id ADC EAX,imm32 Add with carry imm32 to EAX
;;   80 /2 ib ADC r/m8,imm8 Add with carry imm8 to r/m8
;;   81 /2 iw ADC r/m16,imm16 Add with carry imm16 to r/m16
;;   81 /2 id ADC r/m32,imm32 Add with CF imm32 to r/m32
;;   83 /2 ib ADC r/m16,imm8 Add with CF sign-extended imm8 to r/m16
;;   83 /2 ib ADC r/m32,imm8 Add with CF sign-extended imm8 into r/m32
;;   10 /r ADC r/m8,r8 Add with carry byte register to r/m8
;;   11 /r ADC r/m16,r16 Add with carry r16 to r/m16
;;   11 /r ADC r/m32,r32 Add with CF r32 to r/m32
;;   12 /r ADC r8,r/m8 Add with carry r/m8 to byte register
;;   13 /r ADC r16,r/m16 Add with carry r/m16 to r16
;;   13 /r ADC r32,r/m32 Add with CF r/m32 to r32

;;   (04 ib) (ADD AL,imm8) (Add imm8 to AL)
;;   05 iw ADD AX,imm16 Add imm16 to AX
;;   05 id ADD EAX,imm32 Add imm32 to EAX
;;   80 /0 ib ADD r/m8,imm8 Add imm8 to r/m8
;;   81 /0 iw ADD r/m16,imm16 Add imm16 to r/m16
;;   81 /0 id ADD r/m32,imm32 Add imm32 to r/m32
;;   83 /0 ib ADD r/m16,imm8 Add sign-extended imm8 to r/m16
;;   83 /0 ib ADD r/m32,imm8 Add sign-extended imm8 to r/m32
;;   00 /r ADD r/m8,r8 Add r8 to r/m8
;;   01 /r ADD r/m16,r16 Add r16 to r/m16
;;   01 /r ADD r/m32,r32 Add r32 to r/m32
;;   02 /r ADD r8,r/m8 Add r/m8 to r8
;;   03 /r ADD r16,r/m16 Add r/m16 to r16
;;   03 /r ADD r32,r/m32 Add r/m32 to r32 )
