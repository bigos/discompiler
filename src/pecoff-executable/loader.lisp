(in-package :discompiler)

(defun loader (bytes &optional (module))
  (let ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001)))
    ;;(format t "module arg>>>>  ~s~%" module)
    (setf module (allocate-and-load-sections bytes mem module))
    (report-loader-errors bytes mem)
    (push module (modules mem))
    (values
     mem
     module)))

(defun report-loader-errors (bytes mem)
  (if (zerop (optional-header-value bytes "Import Table RVA"))
      (princ " zero import RVA detected")
      (imported-functions bytes mem))
  (if (zerop (optional-header-value bytes "IAT RVA"))
      (princ " zero IAT rva detected ")))

;; try to write recursive loader
(defun loader-w (file)
  (declare (optimize (debug 3) (safety 3)))
  (let ((my-module (make-module))
        (mem (make-instance 'memory :start #x110000 :end #xFFFF0001))
        (bytes (file-to-bytes file)))
    (setf (module-fulldllname my-module) file)
    (setf (module-basedllname my-module) (filename file))
    (setf my-module (allocate-and-load-sections bytes mem my-module))
    (unless (zerop (optional-header-value bytes "Import Table RVA"))
      (imported-functions bytes mem))
    (push my-module (modules mem))
    ;;(values mem my-module)
    (loader bytes my-module)
    ))

(defun filename (path)
  (pathname-name path))

;; Windows Internals Part 1 (6th Edition)
;; page 234
;; Early Process Initialization
;;
;; page 238
;; Loaded Module Database
;;
;; page 242
