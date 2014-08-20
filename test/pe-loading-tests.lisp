(in-package :discompiler)

(in-suite :loading)

(test test-executable-integrity
  (is (equalp #(17 122 62 7 172 101 207 43 236 55 231 193 95 182 209 19)
              (md5:md5sum-file
               "~/discompiler/SampleExecutables/PE/crackme12.exe"))))

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
