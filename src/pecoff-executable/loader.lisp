(in-package :discompiler)

(defun loader (bytes)
  (let ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001)))
    ;;(format t "module arg>>>>  ~s~%" module)
    (allocate-and-load-sections bytes mem (dll-base bytes mem))
    (report-loader-errors bytes mem)
    (imported-functions bytes mem)
    mem))

(defun report-loader-errors (bytes mem)
  (when (zerop (optional-header-value bytes "Import Table RVA"))
      (princ " zero import RVA detected"))
  (when (zerop (optional-header-value bytes "IAT RVA"))
      (princ " zero IAT rva detected ")))

(defun set-module-data (module bytes dll-base)
  (setf (module-originalbase module) (image-base bytes)
        (module-sizeofimage module) (size-of-image bytes)
        (module-dllbase module) dll-base )
  module)

(defparameter *recursion-level* 0)

;; try to write recursive loader
(defun recursive-loader (file)
  (let ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001))
        (bytes (file-to-bytes file)))
    (setf *recursion-level* 0)
    (loader-1 file mem bytes)))

(defun loader-1 (file mem bytes)
  (declare (optimize (debug 3) (safety 3)))
  (incf *recursion-level*)
  (when (> *recursion-level* 10) (error "loader recursion too deep"))
  (let ((module (make-module)))
    (setf (module-fulldllname module) file
          (module-basedllname module) (filename file)
          (module-originalbase module) (image-base bytes)
          (module-sizeofimage module) (size-of-image bytes)
          (module-dllbase module) (dll-base bytes mem))
    (allocate-and-load-sections bytes mem (dll-base bytes mem))
    (report-loader-errors bytes mem)
    (push module (modules mem))
    (imported-functions bytes mem)
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
