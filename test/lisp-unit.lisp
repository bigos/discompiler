(in-package :discompiler)

(setf *print-failures* t)

;; *** leftover tests from lisp-unit ***

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
