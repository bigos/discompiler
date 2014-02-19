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
    ;;compare file and loaded pe header
    (is (equalp (get-allocated-bytes mem (image-base bytes) (length-of-pe-header bytes))
                (subseq bytes 0 (length-of-pe-header bytes))))
    (is (equalp #(#x4d #x5a) (get-allocated-bytes mem #x400000 2))) ;signature
    (is (equalp #(#xaa #xdf #x87 #x50) (get-allocated-bytes mem #x400100 4))) ;TimeDateSatamp
    (is (equalp #(#x40 #x0 #x0 #x42 ) (get-allocated-bytes mem #x4002b4 4))) ;last used bytes
    (is (equalp #(0 0 0 0) (get-allocated-bytes mem #x4002b8 4))) ; padding with zeros
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
