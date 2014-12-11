;; This file is for temporary Lisp code, for situations where REPL is not convenient enough
;; Once the code matures, it should go to source folder.

(defun investigate ()
  (declare (debug 3))
  (let* ((file "~/discompiler/SampleExecutables/PE/myfavlibrary.exe")
         (bytes (file-to-bytes file))
         (mem (loader bytes))
         (imports (imported-functions bytes mem)))
    (cerror "aaa" "bbb")
    (format t "~&~A~%" (nth 17 imports))))

;;;
