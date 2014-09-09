(in-package :discompiler)

(setf *test* T)

(in-suite :loading)
(test loaded-modules
  (let ((file "~/discompiler/SampleExecutables/PE/myfavlibrary.exe"))
    (sb-ext:gc :full t)
    (multiple-value-bind (mem my-module) (recursive-loader file)
      (declare (ignore mem))
      (is (equalp (module-fulldllname my-module) file))
      (is (equalp (module-basedllname my-module) "myfavlibrary"))
      (is (equalp (module-dllbase my-module) #x400000))
      (is (equalp (module-originalbase my-module) #x400000))
      (is (equalp (module-sizeofimage  my-module) #xd57000))
      )
    (sb-ext:gc :full t)))

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
               "~/discompiler/SampleExecutables/PE/myfavlibrary.exe")))

  (is (equalp #(120 189 200 156 93 158 32 98 9 190 197 165 167 63 145 247)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/GdiPlus.dll")))
  (is (equalp #(26 255 36 76 161 52 149 108 84 71 79 78 36 51 228 206)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/advapi32.dll")))
  (is (equalp #(167 125 251 133 250 238 73 214 108 116 218 96 36 235 198 155)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/comctl32.dll")))
  (is (equalp #(30 219 27 184 157 2 25 85 230 247 38 89 17 23 91 141)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/comdlg32.dll")))
  (is (equalp #(245 174 225 51 191 68 82 24 82 129 156 34 2 216 36 83)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/gdi32.dll")))
  (is (equalp #(135 202 124 230 70 149 119 240 89 41 123 157 101 86 214 109)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/imm32.dll")))
  (is (equalp #(136 129 144 227 20 85 250 215 147 49 47 141 8 113 70 235)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/kernel32.dll")))
  (is (equalp #(44 254 128 170 52 40 192 158 109 230 127 172 80 218 101 207)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/mpr.dll")))
  (is (equalp #(142 224 182 237 205 95 230 59 222 238 184 35 81 177 16 238)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/msacm.dll")))
  (is (equalp #(181 51 31 43 111 55 198 108 41 200 71 243 185 79 249 0)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/msimg32.dll")))
  (is (equalp #(176 254 250 129 109 97 236 102 170 118 93 223 83 78 171 94)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/msvcrt.dll")))
  (is (equalp #(187 92 191 252 9 100 151 80 97 103 188 225 217 105 14 242)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/ntdll.dll")))
  (is (equalp #(79 233 217 250 98 208 32 227 94 10 198 209 174 235 150 240)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/ole32.dll")))
  (is (equalp #(95 45 190 60 181 99 116 28 128 132 101 123 249 86 206 100)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/oleacc.dll")))
  (is (equalp #(179 116 45 238 133 139 36 62 119 199 61 43 143 124 130 35)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/oleaut32.dll")))
  (is (equalp #(156 212 195 62 33 21 228 239 247 131 106 218 86 40 71 214)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/oledlg.dll")))
  (is (equalp #(150 228 140 126 185 8 157 29 191 111 133 202 17 178 100 223)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/psapi.dll")))
  (is (equalp #(133 122 232 66 229 119 145 148 89 92 26 166 66 134 144 162)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/rpcrt4.dll")))
  (is (equalp #(213 152 138 80 72 228 220 113 117 188 169 242 159 193 68 174)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/shell32.dll")))
  (is (equalp #(92 32 30 151 65 187 64 175 96 167 198 109 43 58 252 196)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/shlwapi.dll")))
  (is (equalp #(199 38 97 248 85 42 206 124 92 133 225 106 60 245 5 196)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/user32.dll")))
  (is (equalp #(44 222 73 102 102 169 117 162 206 143 150 159 48 66 200 219)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/uxtheme.dll")))
  (is (equalp #(211 132 8 150 123 231 56 208 193 180 112 5 188 232 206 235)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/version.dll")))
  (is (equalp #(192 130 63 197 70 150 99 186 99 231 219 136 249 145 157 112)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/wininet.dll")))
  (is (equalp #(144 253 170 34 243 141 158 145 31 145 250 59 138 31 126 93)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/winmm.dll")))
  (is (equalp #(119 126 178 157 1 53 216 26 217 130 138 43 5 68 52 150)
              (md5:md5sum-file  "~/discompiler/SampleExecutables/PE/DLLs/winspool.drv"))))

(test test-old-firefox
  (let* ((file  "~/discompiler/SampleExecutables/PE/firefox.exe")
         (bytes (file-to-bytes file))
         (mem (loader bytes))
         (file-beginning #(77 90 144 0 3 0 0 0 4 0 0 0 255 255 0 0 184 0 0 0 0 0 0 0 64 0 0 0 0 0 0 0 0
                              0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 14 31 186 14 0
                              180 9 205 33 184 1 76 205 33 84 104 105 115 32 112 114 111 103 114 97 109 32
                              99 97 110 110 111 116 32 98 101 32 114 117 110 32 105 110 32 68 79 83 32 109
                              111 100 101 46 13 13 10 36 0 0 0 0 0 0 0 83 217 24 181 23 184 118 230 23 184
                              118 230 23 184 118 230 128 124 8 230 22 184 118 230 48 126 13 230 19 184 118
                              230 48 126 24 230 20 184 118 230 48 126 11 230 21 184 118 230 48 126 27 230 2
                              184 118 230 30 192 229 230 18 184 118 230 23 184 119 230 67 184 118 230 48
                              126 0 230 31 184 118 230 48 126 10 230 22 184 118 230 48 126 14 230 22 184
                              118 230 82 105 99 104 23 184 118 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                              0 0 0 0 80 69 0 0 76 1 5 0 68 204 94 79 0 0 0 0 0 0 0 0 224 0 34 1 11 1 8 0 0
                              32 0 0 0 208 13 0 0 0 0 0 249 27 0 0 0 16 0 0 0 48 0 0 0 0 64 0 0 16 0 0 0 16
                              0 0 4 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0 14 0 0 16 0 0 119 106 14 0 2 0 64 1 0
                              0 16 0 0 16 0 0 0 0 4 0 0 16 0 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 0 0 116 56 0 0
                              80 0 0 0 0 80 0 0 48 152 13 0 0 0 0 0 0 0 0 0 0 0 14 0 184 27 0 0 0 240 13 0
                              148 2 0 0 96 49 0 0 28 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                              72 55 0 0 64 0 0 0 0 0 0 0 0 0 0 0 0 48 0 0 60 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                              0 0 0 0 0 0 0 0 0 0 0 0 46 116 101 120 116 0 0 0 108 24 0 0 0 16 0 0 0 32 0 0
                              0 16 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 0 0 96 46 114 100 97 116 97 0 0 240 14 0
                              0 0 48 0 0 0 16 0 0 0 48 0 0 0 0 0 0 0 0 0 0 0 0 0 0 64 0 0 64 46 100 97 116
                              97 0 0 0 188 4 0 0 0 64 0 0 0 16 0 0 0 64 0 0 0 0 0 0 0 0 0 0 0 0 0 0 64 0 0
                              192 46 114 115 114 99 0 0 0 48 152 13 0 0 80 0 0 0 160 13 0 0 80 0 0 0 0 0 0
                              0 0 0 0 0 0 0 0 64 0 0 64 46 114 101 108 111 99 0 0 78 12 0 0 0 240 13 0 0 16
                              0 0 0 240 13 0 0 0 0 0 0 0 0 0 0 0 0 0 64 0 0 66)))
    (is (equalp (md5:md5sum-file file)
                #(99 127 43 220 14 83 112 77 18 29 221 39 161 246 32 144)))
    (is (equalp
         (subseq  bytes 0 (length file-beginning))
         file-beginning))
    (is (equalp file-beginning
                (get-allocated-bytes mem #x400000 (length file-beginning))))))

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
         (mem (loader bytes)))
    ;; (allocate-and-load-sections bytes mem)
    ;;compare file and loaded pe header
    (is (equalp (get-allocated-bytes mem (image-base bytes) (length-of-pe-header bytes))
                (subseq bytes 0 (length-of-pe-header bytes))))
    (is (equalp #(#x4d #x5a) (get-allocated-bytes mem #x400000 2))) ;signature
    (is (equalp #(#xaa #xdf #x87 #x50) (get-allocated-bytes mem #x400100 4))) ;TimeDateSatamp
    (is (equalp #(0 0 #x40 #x0 #x0 #x42 ) (get-allocated-bytes mem #x4002b2 6))) ;last used bytes

    ;; there's a problem with bytes loaded
    ;; (format t "~S~%~S~%~S~%" (get-allocated-bytes mem  #x400000 700)
    ;;         (subseq bytes 0 700)
    ;;         (length-of-pe-header bytes))
    (is (equalp
         (subseq bytes 0 700)
         #(77 90 144 0 3 0 0 0 4 0 0 0 255 255 0 0 184 0 0 0 0 0 0 0 64 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 248 0 0 0 14 31 186 14
           0 180 9 205 33 184 1 76 205 33 84 104 105 115 32 112 114 111 103 114 97 109
           32 99 97 110 110 111 116 32 98 101 32 114 117 110 32 105 110 32 68 79 83 32
           109 111 100 101 46 13 13 10 36 0 0 0 0 0 0 0 220 210 93 110 152 179 51 61 152
           179 51 61 152 179 51 61 145 203 183 61 153 179 51 61 131 46 173 61 170 179 51
           61 11 253 171 61 192 179 51 61 145 203 176 61 140 179 51 61 145 203 160 61
           181 179 51 61 152 179 50 61 244 176 51 61 131 46 153 61 65 179 51 61 131 46
           152 61 234 180 51 61 131 46 169 61 153 179 51 61 131 46 174 61 153 179 51 61
           82 105 99 104 152 179 51 61 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 80 69 0 0 76 1 5
           0 170 223 135 80 0 0 0 0 0 0 0 0 224 0 2 1 11 1 10 0 0 24 113 0 0 34 98 0 0 0
           0 0 113 136 95 0 0 16 0 0 0 48 113 0 0 0 64 0 0 16 0 0 0 2 0 0 5 0 1 0 0 0 0
           0 5 0 1 0 0 0 0 0 0 112 213 0 0 4 0 0 152 237 211 0 2 0 0 129 0 0 16 0 0 16 0
           0 0 0 16 0 0 16 0 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 0 0 184 193 143 0 204 1 0 0
           0 0 154 0 192 181 47 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 192 201 0 12 196 8 0
           112 89 113 0 28 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 224 90
           130 0 64 0 0 0 0 0 0 0 0 0 0 0 0 48 113 0 144 11 0 0 0 0 0 0 0 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0 0 0 46 116 101 120 116 0 0 0 5 22 113 0 0 16 0 0 0 24 113
           0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 0 0 96 46 114 100 97 116 97 0 0 160 208
           30 0 0 48 113 0 0 210 30 0 0 28 113 0 0 0 0 0 0 0 0 0 0 0 0 0 64 0 0 64 46
           100 97 116 97 0 0 0 224 236 9 0 0 16 144 0 0 242 7 0 0 238 143 0 0 0 0 0 0 0
           0 0 0 0 0 0 64 0 0 192 46 114 115 114 99 0 0 0 192 181 47 0 0 0 154 0 0 182
           47 0 0 224 151 0 0 0 0 0 0 0 0 0 0 0 0 0 64 0 0 64 46 114 101 108 111 99 0 0
           54 166 11 0 0 192 201 0 0 168 11 0 0 150 199 0 0 0 0 0 0 0 0 0 0 0 0 0 64 0 0
           66 0 0 0 0)))
    (is (equalp
         (get-allocated-bytes mem  #x400000 700)
         #(77 90 144 0 3 0 0 0 4 0 0 0 255 255 0 0 184 0 0 0 0 0 0 0 64 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 248 0 0 0 14
           31 186 14 0 180 9 205 33 184 1 76 205 33 84 104 105 115 32 112 114 111
           103 114 97 109 32 99 97 110 110 111 116 32 98 101 32 114 117 110 32 105
           110 32 68 79 83 32 109 111 100 101 46 13 13 10 36 0 0 0 0 0 0 0 220 210
           93 110 152 179 51 61 152 179 51 61 152 179 51 61 145 203 183 61 153 179
           51 61 131 46 173 61 170 179 51 61 11 253 171 61 192 179 51 61 145 203
           176 61 140 179 51 61 145 203 160 61 181 179 51 61 152 179 50 61 244 176
           51 61 131 46 153 61 65 179 51 61 131 46 152 61 234 180 51 61 131 46 169
           61 153 179 51 61 131 46 174 61 153 179 51 61 82 105 99 104 152 179 51 61
           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 80 69 0 0 76 1 5 0 170 223 135 80 0 0 0
           0 0 0 0 0 224 0 2 1 11 1 10 0 0 24 113 0 0 34 98 0 0 0 0 0 113 136 95 0
           0 16 0 0 0 48 113 0 0 0 64 0 0 16 0 0 0 2 0 0 5 0 1 0 0 0 0 0 5 0 1 0 0
           0 0 0 0 112 213 0 0 4 0 0 152 237 211 0 2 0 0 129 0 0 16 0 0 16 0 0 0 0
           16 0 0 16 0 0 0 0 0 0 16 0 0 0 0 0 0 0 0 0 0 0 184 193 143 0 204 1 0 0 0
           0 154 0 192 181 47 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 192 201 0 12 196
           8 0 112 89 113 0 28 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
           0 224 90 130 0 64 0 0 0 0 0 0 0 0 0 0 0 0 48 113 0 144 11 0 0 0 0 0 0 0
           0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 46 116 101 120 116 0 0 0 5 22 113
           0 0 16 0 0 0 24 113 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 32 0 0 96 46 114
           100 97 116 97 0 0 160 208 30 0 0 48 113 0 0 210 30 0 0 28 113 0 0 0 0 0
           0 0 0 0 0 0 0 0 64 0 0 64 46 100 97 116 97 0 0 0 224 236 9 0 0 16 144 0
           0 242 7 0 0 238 143 0 0 0 0 0 0 0 0 0 0 0 0 0 64 0 0 192 46 114 115 114
           99 0 0 0 192 181 47 0 0 0 154 0 0 182 47 0 0 224 151 0 0 0 0 0 0 0 0 0 0
           0 0 0 64 0 0 64 46 114 101 108 111 99 0 0 54 166 11 0 0 192 201 0 0 168
           11 0 0 150 199 0 0 0 0 0 0 0 0 0 0 0 0 0 64 0 0 66 0 0 0 0)))

    (is (eq 696 (length-of-pe-header bytes)))
    (is (equalp #(0 0 0 0) (get-allocated-bytes mem #x4002b8 4))) ; padding with zeros
    (is (equalp #(#x55 #x8b #xec) (get-allocated-bytes mem #x401000 3)))
    ;; following test checks for data before modification by loader during import
    (is (equalp #(20 207 143) (get-allocated-bytes mem #xb13000 3)))
    (is (equalp #(#x28 #x63 #xc1) (get-allocated-bytes mem #xd01000 3)))
    (is (equalp #(#x00 #x00 #x00) (get-allocated-bytes mem #xda0000 3)))
    (is (equalp #(#x00 #x10 #x00) (get-allocated-bytes mem #x109c000 3)))
    ))


(test test-load-sample-file
  (let* ((file (concatenate 'string (project-path) "SampleExecutables/PE/crackme12.exe"))
         (libraries (concatenate 'string (project-path) "SampleExecutables/PE/DLLs/"))
         (bytes (file-to-bytes file))
         (mem (make-instance 'memory :start #x110000 :end  #xFFFF0001))
         (base) (size-header) (section-alignment)
         (new-mem))
    (is (equalp '((#x110000 . #xffff0000)) (find-free mem)))
    (is (eq #x400000 (setf base (image-base bytes))))
    (is (eq 1024 (struct-value "SizeOfHeaders" (optional-header bytes))))
    (is (eq 1024 (optional-header-value bytes "SizeOfHeaders")))
    (is (eq 4096 (setf size-header (aligned-size
                                    (optional-header-value bytes "SizeOfHeaders")
                                    (optional-header-value bytes "SectionAlignment")))))
    (is (eq 4096 (setf section-alignment (optional-header-value bytes "SectionAlignment"))))

    (allocate-and-load-sections bytes mem (dll-base bytes mem) )
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

    ;; new tests  for new loader
    (setf new-mem (init-recursive-loader file))

    ;; (is (equalp '((#x400000 . #x400fff)
    ;;               (#x401000 . #x401fff)
    ;;               (#x402000 . #x402fff)
    ;;               (#x403000 . #x403fff)
    ;;               (#x404000 . #x404fff)) (butlast (allocated new-mem))))
    ;; (is (equalp '((#x110000 . #x3FFFFF)
    ;;               (#x405000 . #xffff0000)) (find-free new-mem)))

    (is (equalp (nth 0 (modules new-mem))
                (make-module
                 :BASEDLLNAME "crackme12"
                 :DLLBASE #x400000
                 :FULLDLLNAME (concatenate 'string (project-path) "SampleExecutables/PE/crackme12.exe")
                 :ORIGINALBASE #x 400000
                 :SIZEOFIMAGE #x5000)))
    (is (equalp (nth 1 (modules new-mem))
                (make-module
                 :BASEDLLNAME "USER32"
                 :DLLBASE #x77d40000
                 :FULLDLLNAME (pathname (concatenate 'string libraries "user32.dll"))
                 :ORIGINALBASE #x77D40000
                 :SIZEOFIMAGE #x90000)))
    (is (equalp (nth 2 (modules new-mem))
                (make-module
                 :BASEDLLNAME "GDI32"
                 :DLLBASE #x77f10000
                 :FULLDLLNAME (pathname (concatenate 'string libraries "gdi32.dll"))
                 :ORIGINALBASE #x77F10000
                 :SIZEOFIMAGE #x46000)))
    (is (equalp (nth 3(modules new-mem))
                (make-module
                 :BASEDLLNAME "kernel32"
                 :DLLBASE #x7c800000
                 :FULLDLLNAME (pathname (concatenate 'string libraries "kernel32.dll"))
                 :ORIGINALBASE #x7C800000
                 :SIZEOFIMAGE #xF4000)))
    (is (equalp (nth 4 (modules new-mem))
                (make-module
                 :BASEDLLNAME "ntdll"
                 :DLLBASE #x7c900000
                 :FULLDLLNAME (pathname (concatenate 'string libraries "ntdll.dll"))
                 :ORIGINALBASE #x7C900000
                 :SIZEOFIMAGE  #xB0000)))))

(test test-new-loader
  (let* ((file (concatenate 'string (project-path) "SampleExecutables/PE/crackme12.exe"))
         (libraries (concatenate 'string (project-path) "SampleExecutables/PE/DLLs/"))
         (new-mem))

    (setf new-mem (init-recursive-loader file))
    ;; WARNING all modules have the same SIZEOFIMAGE ORIGINALBASE
    ;; (format t "modules found:: ~S~%" (modules new-mem))


    (is (equalp (nth 0 (modules new-mem))
                (make-module
                 :BASEDLLNAME "crackme12"
                 :DLLBASE #x400000
                 :FULLDLLNAME (concatenate 'string (project-path) "SampleExecutables/PE/crackme12.exe" )
                 :ORIGINALBASE #x 400000
                 :SIZEOFIMAGE #x5000)))

    ;; calling (recursive-loader "./SampleExecutables/PE/DLLs/user32.dll")
    ;;  directly in REPL gives correct results
    (is (equalp (nth 1 (modules new-mem))
                (make-module
                 :BASEDLLNAME "USER32"
                 :DLLBASE #x77d40000
                 :FULLDLLNAME (pathname (concatenate 'string libraries "user32.dll"))
                 :ORIGINALBASE #x77D40000
                 :SIZEOFIMAGE #x90000)))
    (is (equalp (nth 2 (modules new-mem))
                (make-module
                 :BASEDLLNAME "GDI32"
                 :DLLBASE #x77f10000
                 :FULLDLLNAME (pathname (concatenate 'string libraries "gdi32.dll"))
                 :ORIGINALBASE #x77F10000
                 :SIZEOFIMAGE #x46000)))
    (is (equalp (nth 3 (modules new-mem))
                (make-module
                 :BASEDLLNAME "kernel32"
                 :DLLBASE #x7c800000
                 :FULLDLLNAME (pathname (concatenate 'string libraries "kernel32.dll"))
                 :ORIGINALBASE #x7C800000
                 :SIZEOFIMAGE #xF4000)))
    (is (equalp (nth 4 (modules new-mem))
                (make-module
                 :BASEDLLNAME "ntdll"
                 :DLLBASE #x7c900000
                 :FULLDLLNAME (pathname (concatenate 'string libraries "ntdll.dll"))
                 :ORIGINALBASE #x7C900000
                 :SIZEOFIMAGE  #xB0000)))
    ))

(test test-shorter-memory-map
  (let*  ((file (concatenate 'string (project-path) "SampleExecutables/PE/crackme12.exe"))
          (libraries (concatenate 'string (project-path) "SampleExecutables/PE/DLLs/"))
          (mem) (mem-map))
   ))
