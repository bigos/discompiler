(defpackage #:discompiler-test
  (:use :cl :discompiler :fiveam))

(in-package :discompiler-test)

(def-suite :example-suite)
(in-suite :example-suite)

(test test-addition
      "test simple addition"
      (is (= 3 (+ 1 2)))
      (is (= 1 (+ 0 1)))
      (is (= 17 (1- (* 3 6))))
      (is (= 6 (/ 6 1)))
      (is (= 6 (+ 1 2 3))))

(test add-2
  "Test the ADD-2 function" ;; a short description
  ;; the checks
  (is (= 2 (+ 2 (- 5 5))))
  (is (= 0 (+ 2 -2))))


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
