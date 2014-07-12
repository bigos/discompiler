(in-package :discompiler)

(defun loader (bytes)
  (let ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001)))
    ;;(format t "module arg>>>>  ~s~%" module)
    (allocate-and-load-sections bytes mem (dll-base bytes mem))
    (report-loader-errors bytes mem)
    (imported-functions bytes mem)
    mem))

(defun report-loader-errors (file bytes mem)
  (when (zerop (optional-header-value bytes "Import Table RVA"))
    (format t "~%~A ~%" file )
    (princ " zero import RVA detected")
    (terpri))
  (when (zerop (optional-header-value bytes "IAT RVA"))
    (format t "~%~A ~%" file )
    (princ " zero IAT rva detected ")
    (terpri)))

(defun set-module-data (module bytes dll-base)
  (setf (module-originalbase module) (image-base bytes)
        (module-sizeofimage module) (size-of-image bytes)
        (module-dllbase module) dll-base )
  module)

(defparameter *recursion-level* 0)

;; try to write recursive loader
(defun init-recursive-loader (file)
  (setf *recursion-level* 0)
  (recursive-loader file))

(defun recursive-loader (file)
  (let ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001))
        (bytes (file-to-bytes file)))
    (loader-1 file mem bytes)))

(defun loader-1 (file mem bytes)
  (declare (optimize (debug 3) (safety 3)))
  (incf *recursion-level*)
  (when (> *recursion-level* 100) (error "loader recursion too deep"))
  (let ((module (make-module)))
    (setf (module-fulldllname module) file
          (module-basedllname module) (filename file)
          (module-originalbase module) (image-base bytes)
          (module-sizeofimage module) (size-of-image bytes)
          (module-dllbase module) (dll-base bytes mem))
    (allocate-and-load-sections bytes mem (dll-base bytes mem))
    (push  module (modules mem))
    (imported-functions bytes mem)
     (report-loader-errors file bytes mem)
    (values mem module)))

(defun filename (path)
  (pathname-name path))

(defun full-filename (path)
  (format nil
          "~a.~a"
          (pathname-name path)
          (pathname-type path)))

(defun dll-base (bytes memory)
  (car (find-next-free-block memory
                             (size-of-image bytes)
                             (image-base bytes))))

;; Windows Internals Part 1 (6th Edition)
;; page 234
;; Early Process Initialization
;;
;; page 238
;; Loaded Module Database
;;
;; page 242
