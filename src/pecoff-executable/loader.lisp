(in-package :discompiler)

(defun loader (bytes &optional module)
  (let ((mem (make-instance 'memory :start #x110000 :end #xFFFF0001)))
    ;;(format t "module arg>>>>  ~s~%" module)
    (when module
      (setf module  (set-module-data module bytes (dll-base bytes mem))))
    (allocate-and-load-sections bytes mem (dll-base bytes mem))
    (if (zerop (optional-header-value bytes "Import Table RVA"))
        (princ " zero import RVA detected")
        (imported-functions bytes mem))
    (if (zerop (optional-header-value bytes "IAT RVA"))
        (princ " zero IAT rva detected "))
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

(defun set-module-data (module bytes dll-base)
  (setf (module-originalbase module) (image-base bytes)
        (module-sizeofimage module) (size-of-image bytes)
        (module-dllbase module) dll-base )
  module)

;; try to write recursive loader
(defun loader-w (file)
  (declare (optimize (debug 3) (safety 3)))
  (let ((my-module (make-module))
        (mem (make-instance 'memory :start #x110000 :end #xFFFF0001))
        (bytes (file-to-bytes file)))
    (setf (module-fulldllname my-module) file)
    (setf (module-basedllname my-module) (filename file))
    (setf my-module (set-module-data my-module bytes (dll-base bytes mem))
          )
    (allocate-and-load-sections bytes mem (dll-base bytes mem))
    (unless (zerop (optional-header-value bytes "Import Table RVA"))
      (imported-functions bytes mem))
    (push my-module (modules mem))
    ;;(values mem my-module)
    (loader bytes my-module)
    ))

(defun filename (path)
  (pathname-name path))

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
