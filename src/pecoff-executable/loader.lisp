(in-package :discompiler)

(defun loader (bytes &optional (module))
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001)))
    ;;(format t "module arg>>>>  ~s~%" module)
    (setf module (allocate-and-load-sections bytes mem module))
    (if (zerop (optional-header-value bytes "Import Table RVA"))
        (princ " zero import RVA detected")
        (imported-functions bytes mem))
    (if (zerop (optional-header-value bytes "IAT RVA"))
        (princ " zero IAT rva detected "))
    (values
     mem
     module)))

(defun loader-w (file)
  (declare (optimize (debug 3) (safety 3) (speed 0)))
  (let ((my-module (make-module))
        (bytes (file-to-bytes file)))
    (setf (module-fulldllname my-module) file)
    (setf (module-basedllname my-module) (filename file))
    (loader bytes my-module)))

(defun filename (path)
  (if (pathname-type path)
      (format nil "~a.~a"
              (pathname-name path)
              (pathname-type path))
      (pathname-name path)))

;; Windows Internals Part 1 (6th Edition)
;; page 234
;; Early Process Initialization
;;
;; page 238
;; Loaded Module Database
;;
;; page 242
