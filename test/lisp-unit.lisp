(in-package :discompiler)

(setf *print-failures* t)

;; add this check what garbage collection is doing
;;(setf  (sb-ext:gc-logfile) "/tmp/sbcl-log-file.log")

(define-test test-executable-integrity
  (assert-equalp #(17 122 62 7 172 101 207 43 236 55 231 193 95 182 209 19)
                 (md5:md5sum-file
                  "~/discompiler/SampleExecutables/crackme12.exe"))
  (assert-equalp #(108 118 94 130 181 127 46 102 206 156 84 172 35 132 113 217)
                 (md5:md5sum-file
                  "~/discompiler/SampleExecutables/ordinal-imports.dll"))
  (assert-equalp #(75 77 21 177 248 104 180 41 239 172 255 187 89 19 216 164)
                 (md5:md5sum-file
                  "~/discompiler/SampleExecutables/myfavlibrary.exe")))

(define-test test-sample-file
  "sample executable file"
  (let* ((file "~/discompiler/SampleExecutables/crackme12.exe")
         (bytes (file-to-bytes file)))
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

(define-test test-imported-libraries
  (let* ((file "~/discompiler/SampleExecutables/myfavlibrary.exe")
         (bytes (file-to-bytes file))
         (mem (loader bytes))
         (imports (imported-functions bytes mem)))
    (sb-ext:gc :full T)
    (assert-eq 22 (length imports))
    (assert-equalp "ADVAPI32.dll" (car (nth 0 imports)))
    (assert-eq 13 (length (cadr (nth 0 imports))))
    (assert-equalp "VERSION.dll" (car (nth 8 imports)))
    (assert-eq 3 (length (cadr (nth 8 imports))))
    (assert-equalp (cadr (nth 8 imports))
                   '((9424324 9431304 (5 . "GetFileVersionInfoSizeW"))
                     (9424328 9431282 (6 . "GetFileVersionInfoW"))
                     (9424332 9431264 (14 . "VerQueryValueW"))))
    (assert-equalp "OLEAUT32.dll" (car (nth 17 imports)))
    (assert-eq 21 (length (cadr (nth 17 imports))))
    (assert-equalp (cadr (nth 17 imports))  ;verify correct import by ordinal
                   #((9423116 2147483650 (2 . "SysAllocString"))
                     (9423120 2147483652 (4 . "SysAllocStringLen"))
                     (9423124 2147483664 (16 . "SafeArrayDestroy"))
                     (9423128 2147483672 (24 . "SafeArrayUnaccessData"))
                     (9423132 2147483671 (23 . "SafeArrayAccessData"))
                     (9423136 2147484059 (411 . "SafeArrayCreateVector"))
                     (9423140 2147483654 (6 . "SysFreeString"))
                     (9423144 2147483656 (8 . "VariantInit"))
                     (9423148 2147483660 (12 . "VariantChangeType"))
                     (9423152 2147484068 (420 . "OleCreateFontIndirect"))
                     (9423156 2147483762 (114 . "VarBstrFromDate"))
                     (9423160 2147483655 (7 . "SysStringLen"))
                     (9423164 2147483832 (184 . "SystemTimeToVariantTime"))
                     (9423168 2147483833 (185 . "VariantTimeToSystemTime"))
                     (9423172 2147483657 (9 . "VariantClear"))
                     (9423176 2147483658 (10 . "VariantCopy"))
                     (9423180 2147483663 (15 . "SafeArrayCreate"))
                     (9423184 2147483665 (17 . "SafeArrayGetDim"))
                     (9423188 2147483666 (18 . "SafeArrayGetElemsize"))
                     (9423192 2147483668 (20 . "SafeArrayGetLBound"))
                     (9423196 2147483667 (19 . "SafeArrayGetUBound"))))
    (assert-equalp "WINMM.dll" (car (nth 21 imports)))
    (assert-eq 10 (length (cadr (nth 21 imports))))
    )
  (sb-ext:gc :full T)
  )

(define-test test-ordinal-exports
  (let* ((file "~/discompiler/SampleExecutables/ordinal-imports.dll")
         (bytes (file-to-bytes file))
         (memory (loader bytes))
         (export-list (exports bytes memory)))
    (sb-ext:gc :full T)
    (assert-equalp "6FC377EA" (address-to-code export-list 0))
    (assert-equalp "6FC3742E" (address-to-code export-list 1))
    (assert-equalp "6FC64113" (address-to-code export-list 399))
    (assert-error 'type-error (address-to-code export-list 400))

    (assert-equalp "SysAllocString" (ordinal-name export-list 2))
    (assert-equalp "BSTR_UserFree" (ordinal-name export-list 286))
    (assert-equalp "BSTR_UserMarshal" (ordinal-name export-list 284))
    (assert-equalp "OaEnablePerUserTLibRegistration" (ordinal-name export-list 444))
    (assert-equalp "OACleanup" (ordinal-name export-list 500))

    (assert-equalp "6FC34642" (int-to-hex (ordinal-code-address export-list 2)))
    (assert-equalp "6FC50B81" (int-to-hex (ordinal-code-address export-list 3)))
    (assert-equalp "6FC33F0B" (int-to-hex (ordinal-code-address export-list 500)))
    )
  (sb-ext:gc :full T)
  )

(define-test test-load-myfavlibrary
  (let* ((file "~/discompiler/SampleExecutables/myfavlibrary.exe")
         (bytes (file-to-bytes file))
         (mem (make-instance 'memory :start #x110000 :end  #xFFFF0001 :file-bytes bytes)))
    (allocate-and-load-sections bytes mem)
    (sb-ext:gc :full T)
    (assert-equalp #(#x55 #x8b #xec) (get-allocated-bytes mem #x401000 3))
    ;; following test checks for data before modification by loader during import
    (assert-equalp #(20 207 143) (get-allocated-bytes mem #xb13000 3))
    (assert-equalp #(#x28 #x63 #xc1) (get-allocated-bytes mem #xd01000 3))
    (assert-equalp #(#x00 #x00 #x00) (get-allocated-bytes mem #xda0000 3))
    (assert-equalp #(#x00 #x10 #x00) (get-allocated-bytes mem #x109c000 3))
    )
  (sb-ext:gc :full T)
  )

(define-test test-load-sample-file
  (let* ((file "~/discompiler/SampleExecutables/crackme12.exe")
         (bytes (file-to-bytes file))
         (mem (make-instance 'memory :start #x110000 :end  #xFFFF0001 :file-bytes bytes))
         (base) (size-header) (section-alignment)  )
    (assert-equalp '((#x110000 . #xffff0000)) (find-free mem))
    (assert-eq #x400000 (setf base (image-base bytes)))
    (assert-eq 1024 (struct-value "SizeOfHeaders" (optional-header bytes)))
    (assert-eq 1024 (optional-header-value bytes "SizeOfHeaders"))
    (assert-eq 4096 (setf size-header (aligned-size
                                       (optional-header-value bytes "SizeOfHeaders")
                                       (optional-header-value bytes "SectionAlignment"))))
    (assert-eq base (allocate-preferred-block mem size-header base))
    (assert-equalp '((#x110000 . #x3FFFFF) (#x401000 . #xffff0000)) (find-free mem))
    (assert-eq 4096 (setf section-alignment (optional-header-value bytes "SectionAlignment")))
    ;; load sections first
    (allocate-and-load-sections bytes mem)
    (sb-ext:gc :full T)
    ;; check allocation
    (assert-equalp '((#x110000 . #x3FFFFF)
                     (#x405000 . #xffff0000)) (find-free mem)) ;; verify if find-free returns correct values
    (assert-equalp '((#x400000 . #x400fff)
                     (#x401000 . #x401fff)
                     (#x402000 . #x402fff)
                     (#x403000 . #x403fff)
                     (#x404000 . #x404fff)) (butlast (allocated mem)))
    ;; then check first bytes of loaded sections
    (assert-eq #x6a (get-allocated mem #x401000))
    (assert-equalp #(#x6a #x00 #xe8) (get-allocated-bytes mem #x401000 3))
    (assert-eq #x41 (get-allocated mem #x403000))
    (assert-eq #x00 (get-allocated mem #x404000))
    )
  (sb-ext:gc :full T)
  )

(define-test test-allocation
  (let ((mem (make-instance 'memory :start 1 :end 100)))
    (assert-equalp '((100 . 100)) (allocated mem))
    (assert-equalp '((1 . 99)) (find-free mem))
    (assert-equalp 8 (allocate-preferred-block mem 3 8))
    (assert-equalp '((8 . 10) (100 . 100)) (allocated mem))
    (assert-equalp 98 (allocate-preferred-block mem 2 98))
    (assert-equalp '((8 . 10) (98 . 99) (100 . 100)) (allocated mem))
    (assert-equalp nil (allocate-preferred-block mem 3 98))
    (assert-equalp '((8 . 10) (98 . 99) (100 . 100)) (allocated mem))
    ;; check allocated blocks
    (assert-equalp 2 (length (blocks mem)))
    (assert-equalp 3 (length (allocated mem)))
    ;; trying  to deallocate with incorrect block start
    (remove-allocated mem 7)
    ;; should not work
    (assert-equalp 2 (length (blocks mem)))
    (assert-equalp 3 (length (allocated mem)))
    ;; but deallocating giving correct block start
    (remove-allocated mem 8)
    ;; should worky
    (assert-equalp '((98 . 99) (100 . 100)) (allocated mem))
    (assert-equalp 1 (length (blocks mem)))
    (assert-equalp 2 (length (allocated mem)))
    ;; check allocation of first available block
    (assert-equalp '((1 . 97)) (find-free mem))
    (assert-equalp '((98 . 99) (100 . 100)) (allocated mem))
    (assert-equalp  1 (allocate-available-block mem 5))
    (assert-equalp '((1 . 5) (98 . 99) (100 . 100)) (allocated mem))
    (assert-equalp '((6 . 97)) (find-free mem))
    ;; allocation of taken preferred address gives first available
    (assert-equalp 6 (allocate-block mem 9 90))
    ;; allocate available preferred address
    (assert-equalp 90 (allocate-block mem 8 90))
    (assert-equalp '((15 . 89)) (find-free mem)))
  (sb-ext:gc :full T)
  )

(define-test test-block-addressing
  (let ((mem (make-instance 'memory :start 1 :end 100)))
    (assert-equalp 1 (allocate-preferred-block mem 3 1))
    (assert-equalp 5 (allocate-preferred-block mem 3 5))
    ;; insert data into block 0
    (assert-equalp 1 (set-allocated mem 1 11))
    (assert-equalp 2 (set-allocated mem 2 12))
    (assert-equalp 3 (set-allocated mem 3 13))
    ;; test if setting outside of block boundaries works as expected
    ;; need to work out how to check for errors in lisp-unit
    ;; and eventually use errors
    (assert-error 'simple-error (set-allocated mem 0 1))
    (Assert-error 'simple-error (set-allocated mem 4 1))
    (assert-error 'simple-error (set-allocated mem 8 1))
    ;; insert data into block 1
    (assert-equalp 5 (set-allocated mem 5 15))
    (assert-equalp 6 (set-allocated mem 6 26))
    (assert-equalp 7 (set-allocated mem 7 37))
    ;; remove allocated block
    (remove-allocated mem 5)
    ;; test if setting values on deallocated block works as expected
    (assert-error 'simple-error (set-allocated mem 5 15))
    (assert-error 'simple-error (set-allocated mem 7 37))
    ;; probably have to fill newly allocated block with 0s
    (allocate-preferred-block mem 3 5)
    (assert-error 'simple-error (get-allocated mem 4))
    (assert-equalp 0 (get-allocated mem 5))
    (assert-equalp 0 (get-allocated mem 6))
    (assert-equalp 0 (get-allocated mem 7))
    (assert-error 'simple-error (get-allocated mem 8)))
  (sb-ext:gc :full T)
  )

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
