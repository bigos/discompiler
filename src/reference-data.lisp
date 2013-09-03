(in-package :reference-data)

(defparameter *reference-files* (cl-fad:list-directory "my-reference"))

(defun process-file (file)
  (declare (optimize (speed 0) (space 1) (compilation-speed 0) (debug 3)))
  ;; use -> (step (process-file (car *reference-files*)))
  ;; to step through the function
  (let ((lines (file-to-lines file)) (section) (sections) (instructions))
    (dolist (line lines)
      (cond ((blankp line)
             (unless (eq section nil)
                 (setf sections (nconc sections (list section))))
             (setf section nil))
            ((separatorp line)
             (setf sections (nconc sections (list section))
                   instructions (nconc instructions (list sections))
                   sections nil
                   section nil))
            (t
             (setf section (nconc section (list line))))))
    instructions))

(defun file-to-lines (file)
  (let ((lines))
    (with-open-file (stream file)
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (setq lines (nconc lines (list line)))))
    lines))

(defun get-file-instruction (file-no instruction-no)
  (nth instruction-no (process-file (nth file-no *reference-files*))))

(defun process (line)
  (cond ((blankp line)
         (print line))
        ((separatorp line)
         (format t "~&~s ~D~%" line (length line)))
        (t (format t "."))))

(defun blankp (line)
  (cl-ppcre:scan-to-strings "\\A\\z" line))

(defun separatorp (line)
  (cl-ppcre:scan-to-strings "-{10,}" line))
