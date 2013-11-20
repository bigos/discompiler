(in-package :discompiler)


#|
Read first page of the file which includes DOS header, PE header, section
headers etc.

Fetch Image Base address from PE header and determine if that address is
available else allocate another area.  (Case of relocation)

Map the sections into the allocated area

Read information from import table and load the DLLs

Resolve the function addresses and create Import Address Table (IAT).

Create initial heap and stack using values from PE header.

Create main thread and start the process.
|#

(defclass memory ()
  ((allocated :accessor allocated)
   (blocks :accessor blocks :initform nil)
   (start :accessor start :initarg :start)
   (end :accessor end :initarg :end)))

;; need this to be run after creating an object
(defmethod initialize-instance :after ((self memory) &key)
  (setf (allocated self) (list (cons (end self) (end self)))))

(defgeneric find-free (memory))
(defmethod find-free ((self memory))
  (declare (optimize (speed 0) (space 1) (compilation-speed 0) (debug 3)))
  (let ((found-free) (last-allocated) (first-available (start self)))
    (dolist (allocated-block (allocated self))
      (when (not (eq (car allocated-block) first-available))
        (push (cons first-available (1- (car allocated-block))) found-free))
      (setf first-available (1+ (cdr allocated-block)))
      )
    (setf last-allocated (cdar (last (allocated self))))
    (when (< last-allocated (end self))
      (push (cons (1+ last-allocated) (end self)) found-free))
    (reverse found-free)))

(defvar *memory* (make-instance 'memory :start 1 :end 100))

(defun load-in-memory (bytes preferred-addr)
  "simulate loading executable in memory"
  (let ((size (array-total-size bytes)))
   ;
    ;; (if (is-block-available *allocated* preferred-addr (array-total-size bytes))
    ;;     nil
    ;;     ;;(allocate-block preferred-addr size)
    ;;     ;;if available
    ;;     ;;allocate available
    ;;     ;;else raise error
    ;;                                     ;
    ;;     )
    ))

(defgeneric find-free-block (memory size))
(defmethod find-free-block ((self memory) size)
  (dolist (avail (find-free self))
    (if (>= (1+ (- (cdr avail) (car avail))) size)
        (return avail))))

(defgeneric find-preferred-block (memory size preferred))
(defmethod find-preferred-block ((self memory) size preferred)
  (dolist (avail (find-free self))
    (if (and (<= (car avail) preferred)
             (<= (+ preferred size -1) (cdr avail)))
        (return avail))))

(defgeneric allocate-available-block (memory size))
(defmethod allocate-available-block ((self memory) size)
  (let ((found))
    (if (setf found (car (find-free-block self size)))
      (progn
        (push (cons found (+ found size -1 )) (allocated self))
        (push (make-instance 'memory-block :start found :size size) (blocks self))
        (sort (allocated self) #'< :key #'car)))
     found))

(defgeneric allocate-preferred-block (memory size preferred))
(defmethod allocate-preferred-block ((self memory) size preferred)
  (if (find-preferred-block self size preferred)
      (progn
        (push (cons preferred (+ preferred size -1 )) (allocated self))
        (push (make-instance 'memory-block :start preferred :size size) (blocks self))
        (sort (allocated self) #'< :key #'car)
        preferred)
      nil))

(defgeneric allocate-block (memory size preferred))
(defmethod allocate-block ((self memory) size preferred)
    (let ((allocated (allocate-preferred-block self size preferred)))
      (if allocated
          allocated
          (allocate-available-block self size))))

(defgeneric remove-allocated (memory start))
(defmethod remove-allocated ((self memory) start)
  (setf (allocated self)
        (delete start (allocated self)
                :test #'(lambda (ignore item)
                          (if (equalp ignore (car item))
                              T))))
  (setf (blocks self)
        (delete start (blocks self)
                :test #'(lambda (ignore item)
                          (if (equalp ignore (start item))
                              T)))))

(defgeneric get-allocated (memory addr))
(defmethod get-allocated ((self memory) addr)
    (let ((found))
      (dolist (alloc (blocks self))
        (if  (<= (start alloc) addr (end alloc))
             (return (setf found (aref (data alloc) (- addr (start alloc)))))))
      (if found
          found
          (error "address ~S is not valid" addr))))

(defgeneric set-allocated (memory addr val))
(defmethod set-allocated ((self memory) addr val)
  (let ((found))
    (dolist (alloc (blocks self))
      ;;(format t "~S : ~S ~S ~S : ~S ~%" alloc (start alloc) addr (end alloc) (<= (start alloc) addr  (end alloc)))
      (if  (<= (start alloc) addr (end alloc))
           (progn
             (setf (aref (data alloc) (- addr (start alloc))) val)
             (setq found T)
             (return addr))))
    (if found
        addr
        (error "address ~S is not valid" addr))))

(defclass exec ()
  (preferred-address
   obtained-address))
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass memory-block ()
  ((start :reader start :initarg :start)
   (end :accessor end)
   (size :reader size :initarg :size)
   (data :accessor data )))


(defmethod initialize-instance :after ((self memory-block) &key)
  (setf (end self) (+ (size self) (start self) -1))
  (setf (data self) (make-array (1+ (- (end self) (start self)))
                                :initial-element 0)) )
