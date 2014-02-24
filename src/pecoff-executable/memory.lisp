(in-package :discompiler)

(defclass memory ()
  ((allocated :accessor allocated)
   (blocks :accessor blocks :initform nil)
   (start :accessor start :initarg :start)
   (end :accessor end :initarg :end)
   (file-bytes :reader file-bytes :initarg :file-bytes)))

;; need this to be run after creating an object
(defmethod initialize-instance :after ((self memory) &key)
  (setf (allocated self) (list (cons (end self) (end self)))))

(defvar *memory* (make-instance 'memory :start #x110000 :end  #xFFFF0001))

(defgeneric find-free (memory))
(defmethod find-free ((self memory))
  (labels ((range-start (range) (car range))
           (range-end (range) (cdr range))
           (make-range (start end) (cons start end)))
    (let ((last-allocated (cdar (last (allocated self)))))
      (loop
         for allocated-range in (allocated self)
         with last-cons
         with collection
         for new = (cons allocated-range nil)
         and first-available = (start self)
         then (1+ (range-end allocated-range))
         unless (eq (range-start allocated-range) first-available)
         collect (make-range first-available
                             (1- (range-start allocated-range)))
         into found-free
         do
           (if last-cons
               (setf (cdr last-cons) new)
               (setf collection new))
           (setf last-cons new)
           (format t "    ---->>>cc ~S ff~$~%" collection found-free)
         finally
           (format t ">>>cc ~S ff~$~%" collection found-free)
           (when (< last-allocated (end self))
             (setf (cdr last-cons) (make-range (1+ last-allocated) (end self))))
           (return collection)))))

(defgeneric find-free-block (memory size))
(defmethod find-free-block ((self memory) size)
  (dolist (avail (find-free self))
    (when (>= (1+ (- (cdr avail) (car avail))) size)
        (return avail))))

(defgeneric find-next-free-block (memory size preferred))
(defmethod find-next-free-block ((self memory) size preferred)
  (let ((found (find-preferred-block self size preferred)))
    (if found
        found
        (dolist (avail (find-free self))
          (when (and (>= (car avail) preferred)
                   (>= (1+ (- (cdr avail) (car avail))) size))
              (return avail))))))

(defgeneric find-preferred-block (memory size preferred))
(defmethod find-preferred-block ((self memory) size preferred)
  (dolist (avail (find-free self))
    (when (and (<= (car avail) preferred)
             (<= (+ preferred size -1) (cdr avail)))
        (return (cons preferred (cdr avail))))))

(defun allocation-helper (self found size )
  (push (cons found (+ found size -1 )) (allocated self))
  (push (make-instance 'memory-block :start found :size size) (blocks self))
  (sort (allocated self) #'< :key #'car)
  found)

(defgeneric allocate-available-block (memory size))
(defmethod allocate-available-block ((self memory) size)
  (let ((found))
    (if (setf found (car (find-free-block self size)))
        (allocation-helper self found size)
        nil)))

(defgeneric allocate-preferred-block (memory size preferred))
(defmethod allocate-preferred-block ((self memory) size preferred)
  (let ((found))
    (if (setf found (car (find-preferred-block self size preferred)))
          (allocation-helper self found size)
            nil)))

(defgeneric allocate-next-block (memory size preferred))
(defmethod allocate-next-block ((self memory) size preferred)
  (let ((found))
    (if  (setf found (car (find-next-free-block self size preferred)))
         (allocation-helper self found size)
         nil)))

(defgeneric allocate-block (memory size preferred))
(defmethod allocate-block ((self memory) size preferred)
  (let ((allocated (allocate-preferred-block self size preferred)))
    (unless allocated
      (setf allocated (allocate-available-block self size)))
    allocated))

(defmethod remove-allocated ((self memory) start)
  (macrolet ((del (ab fn)
               `(setf (,ab self)
                      (delete start
                              (,ab self)
                              :test #'(lambda (ignore item)
                                        (equalp ignore (,fn item)))))))
    (del allocated car)
    (del blocks start)))

(defgeneric get-allocated (memory addr))
(defmethod get-allocated ((self memory) addr)
  (let ((found))
    (dolist (alloc (blocks self))
      (when  (<= (start alloc) addr (end alloc))
        (return (setf found (aref (data alloc) (- addr (start alloc)))))))
    (if found
        found
        (error "address ~S is not valid" addr))))

(defgeneric get-allocated-bytes (memory addr count))
(defmethod get-allocated-bytes ((self memory) addr count)
  (let ((found))
    (dolist (alloc (blocks self))
      (when  (<= (start alloc) addr (end alloc))
        (return (setf found (subseq (data alloc)
                                    (- addr (start alloc))
                                    (+ (- addr (start alloc)) count))))))
    (if found
        found
        (error "address ~S is not valid" addr))))

(defgeneric get-allocated-string (memory addr))
(defmethod get-allocated-string ((self memory) addr)
  (with-output-to-string (s)
    (loop for x from 0
       for z = (get-allocated self (+ x addr))
       until (zerop z)
       do
         (format s "~a" (code-char z)))))

(defgeneric set-allocated (memory addr val))
(defmethod set-allocated ((self memory) addr val)
  (let ((found))
    (dolist (alloc (blocks self))
      ;;(format t "~S : ~S ~S ~S : ~S ~%" alloc (start alloc) addr (end alloc) (<= (start alloc) addr  (end alloc)))
      (when (<= (start alloc) addr (end alloc))
           (progn
             (setf (aref (data alloc) (- addr (start alloc))) val)
             (setq found T)
             (return addr))))
    (if found
        addr
        (error "address ~S is not valid" addr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass memory-block ()
  ((start :reader start :initarg :start)
   (end :accessor end)
   (size :reader size :initarg :size)
   (data :accessor data )))

(defmethod initialize-instance :after ((self memory-block) &key)
  (setf (end self) (+ (size self) (start self) -1))
  (setf (data self) (make-array (1+ (- (end self) (start self)))
                                :initial-element 0)) )
