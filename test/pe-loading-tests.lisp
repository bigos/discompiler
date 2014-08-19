(in-package :discompiler)

(in-suite :loading)

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
          (mem) (mem-map))))