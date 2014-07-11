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
(defparameter *loaded-modules* nil)

;; try to write recursive loader
(defun init-recursive-loader (file)
  (setf *recursion-level* 0)
  (setf *loaded-modules* nil)
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
    ;; the problem lies with the progrma reading same file bytes all the time
    (format t "ims ~a sizi ~a dllbs ~a file ~a bytes ~A~%"
            (image-base bytes)
            (size-of-image bytes)
            (dll-base bytes mem)
            (filename file)
            (length bytes))
    (setf (module-fulldllname module) file
          (module-basedllname module) (filename file)
          (module-originalbase module) (image-base bytes)
          (module-sizeofimage module) (size-of-image bytes)
          (module-dllbase module) (dll-base bytes mem))
    (format t "modules -> ~A~%" (modules mem))
    (allocate-and-load-sections bytes mem (dll-base bytes mem))
    (push module (modules mem))
    (push module *loaded-modules*)
    (report-loader-errors bytes mem)
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
