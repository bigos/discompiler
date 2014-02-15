(in-package :discompiler)

(in-suite :pe-coff)

(test test-executable-integrity
  (is (equalp #(17 122 62 7 172 101 207 43 236 55 231 193 95 182 209 19)
              (md5:md5sum-file
               "~/discompiler/SampleExecutables/PE/crackme12.exe")))
  (is (equalp #(108 118 94 130 181 127 46 102 206 156 84 172 35 132 113 217)
              (md5:md5sum-file
               "~/discompiler/SampleExecutables/PE/ordinal-imports.dll")))
  (is (equalp #(75 77 21 177 248 104 180 41 239 172 255 187 89 19 216 164)
              (md5:md5sum-file
               "~/discompiler/SampleExecutables/PE/myfavlibrary.exe"))))

(test test-sample-file
  "sample executable file"
  (let* ((file "~/discompiler/SampleExecutables/PE/crackme12.exe")
         (bytes (file-to-bytes file)))
    (is (equal (pe-header-signature-pointer bytes)
               192))
    (is (eq (pe-header-signature-validp bytes)
            T))
    (is (equalp (coff-characteristics bytes)
                '(RELOCS_STRIPPED
                  EXECUTABLE_IMAGE
                  LINE_NUMS_STRIPPED
                  LOCAL_SYMS_STRIPPED
                  32BIT_MACHINE)))
    (is (eq (optional-header-signature bytes)
            #x10b))
    (is (eq (optional-header-image-type bytes)
            'PE32))))

(test imported-libraries
  (let* ((file "~/discompiler/SampleExecutables/PE/myfavlibrary.exe")
         (bytes (file-to-bytes file))
         (mem (loader bytes))
         (imports (imported-functions bytes mem)))
    (is (= 22 (length imports)))
    (is (equalp "ADVAPI32.dll" (car (nth 0 imports))))
    (is (eq 13 (length (cadr (nth 0 imports)))))
    (is (equalp "VERSION.dll" (car (nth 8 imports))))
    (is (eq 3 (length (cadr (nth 8 imports)))))
    (is (equalp (cadr (nth 8 imports))
                '((9424324 9431304 (5 . "GetFileVersionInfoSizeW"))
                  (9424328 9431282 (6 . "GetFileVersionInfoW"))
                  (9424332 9431264 (14 . "VerQueryValueW")))))
    (is (equalp "OLEAUT32.dll" (car (nth 17 imports))))
    (is (eq 21 (length (cadr (nth 17 imports)))))
    (is (equalp (cadr (nth 17 imports)) ;verify correct import by ordinal
                '((9423116 2147483650 (2 . "SysAllocString"))
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
                  (9423196 2147483667 (19 . "SafeArrayGetUBound")))))
    (is (equalp "WINMM.dll" (car (nth 21 imports))))
    (is (eq 10 (length (cadr (nth 21 imports)))))
    (sb-ext:gc :full T)))

(test test-ordinal-exports
  (let* ((file "~/discompiler/SampleExecutables/PE/ordinal-imports.dll")
         (bytes (file-to-bytes file))
         (memory (loader bytes))
         (export-list (exports bytes memory)))
    (is (equalp "6FC377EA" (address-to-code export-list 0)))
    (is (equalp "6FC3742E" (address-to-code export-list 1)))
    (is (equalp "6FC64113" (address-to-code export-list 399)))
    (signals type-error (address-to-code export-list 400))

    (is (equalp "SysAllocString" (ordinal-name export-list 2)))
    (is (equalp "BSTR_UserFree" (ordinal-name export-list 286)))
    (is (equalp "BSTR_UserMarshal" (ordinal-name export-list 284)))
    (is (equalp "OaEnablePerUserTLibRegistration" (ordinal-name export-list 444)))
    (is (equalp "OACleanup" (ordinal-name export-list 500)))

    (is (equalp "6FC34642" (int-to-hex (ordinal-code-address export-list 2))))
    (is (equalp "6FC50B81" (int-to-hex (ordinal-code-address export-list 3))))
    (is (equalp "6FC33F0B" (int-to-hex (ordinal-code-address export-list 500)))))
  (sb-ext:gc :full t))

(test test-load-myfavlibrary
  (let* ((file "~/discompiler/SampleExecutables/PE/myfavlibrary.exe")
         (bytes (file-to-bytes file))
         (mem (make-instance 'memory :start #x110000 :end  #xFFFF0001 :file-bytes bytes)))
    (allocate-and-load-sections bytes mem)
    (is (equalp #(#x55 #x8b #xec) (get-allocated-bytes mem #x401000 3)))
    ;; following test checks for data before modification by loader during import
    (is (equalp #(20 207 143) (get-allocated-bytes mem #xb13000 3)))
    (is (equalp #(#x28 #x63 #xc1) (get-allocated-bytes mem #xd01000 3)))
    (is (equalp #(#x00 #x00 #x00) (get-allocated-bytes mem #xda0000 3)))
    (is (equalp #(#x00 #x10 #x00) (get-allocated-bytes mem #x109c000 3)))
    ))


(test test-load-sample-file
  (let* ((file "~/discompiler/SampleExecutables/PE/crackme12.exe")
         (bytes (file-to-bytes file))
         (mem (make-instance 'memory :start #x110000 :end  #xFFFF0001 :file-bytes bytes))
         (base) (size-header) (section-alignment)  )
    (is (equalp '((#x110000 . #xffff0000)) (find-free mem)))
    (is (eq #x400000 (setf base (image-base bytes))))
    (is (eq 1024 (struct-value "SizeOfHeaders" (optional-header bytes))))
    (is (eq 1024 (optional-header-value bytes "SizeOfHeaders")))
    (is (eq 4096 (setf size-header (aligned-size
                                    (optional-header-value bytes "SizeOfHeaders")
                                    (optional-header-value bytes "SectionAlignment")))))
    (is (eq base (allocate-preferred-block mem size-header base)))
    (is (equalp '((#x110000 . #x3FFFFF) (#x401000 . #xffff0000)) (find-free mem)))
    (is (eq 4096 (setf section-alignment (optional-header-value bytes "SectionAlignment"))))
    ;; load sections first
    (allocate-and-load-sections bytes mem)
    ;; check allocation
    (is (equalp '((#x110000 . #x3FFFFF)
                  (#x405000 . #xffff0000)) (find-free mem))) ;; verify if find-free returns correct values
    (is (equalp '((#x400000 . #x400fff)
                  (#x401000 . #x401fff)
                  (#x402000 . #x402fff)
                  (#x403000 . #x403fff)
                  (#x404000 . #x404fff)) (butlast (allocated mem))))
    ;; then check first bytes of loaded sections
    (is (eq #x6a (get-allocated mem #x401000)))
    (is (equalp #(#x6a #x00 #xe8) (get-allocated-bytes mem #x401000 3)))
    (is (eq #x41 (get-allocated mem #x403000)))
    (is (eq #x00 (get-allocated mem #x404000)))
    ))

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

(test test-addition
  "test simple addition"
  (is (equal 3 (+ 1 2)))
  (is (equal 1 (+ 0 1)))
  (is (equal 17 (1- (* 3 6))))
  (is (equal 6 (/ 6 1)))
  (is (equal 6 (+ 1 2 3))))

(test test-hex-bin
  "converting hex to binary"
  (is (equal "00000000" (int-to-bin 0)))
  (is (equal "11111111" (int-to-bin 255)))
  (is (equal "10101010" (int-to-bin 170))))

(test mod-rm
    "testing correct values of modrm and sib byte fiels"
    (is (equal 1 (mod-part "6b")))
    (is (equal 3 (rm-part "6b")))
    (is (equal 5 (reg-part "6b")))

    (is (equal 1 (ss-part "5d")))
    (is (equal 3 (index-part "5d")))
    (is (equal 5 (base-part "5d")))
    (is (equal 2 (ss-part "a6")))
    (is (equal 4 (index-part "a6")))
    (is (equal 6 (base-part "a6"))))

(test intel-bit-position
  "in Intel documentation bit 0 is the last one and bit 7 is the first one"
  (is (equal 0 (intel-bit-position 7)))
  (is (equal 1 (intel-bit-position 6)))
  (is (equal 2 (intel-bit-position 5)))
  (is (equal 3 (intel-bit-position 4)))
  (is (equal 4 (intel-bit-position 3)))
  (is (equal 5 (intel-bit-position 2)))
  (is (equal 6 (intel-bit-position 1)))
  (is (equal 7 (intel-bit-position 0)))
  ;;(assert-equal 1 "00000001")
  ;;(assert-equal 1 "10000000")
  )
