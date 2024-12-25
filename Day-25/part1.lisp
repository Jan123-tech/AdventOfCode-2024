(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun string-to-string-list (str)
  (mapcar #'string (coerce str 'list)))

(defun create-empty-grid (size)
  (make-array (list size size) :initial-element nil))

(defun place-in-rotated-grid (grid x y element)
  (setf (aref grid x y) element))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

(defun concatenate-strings (nested-lists)
  (mapcar (lambda (inner-list)
            (list (cons (apply #'concatenate 'string (mapcar #'first inner-list)) (list (mapcar #'cdr inner-list)))))
          nested-lists))

(defmacro define-rotate-grid (name new-x-expr new-y-expr)
`(defun ,name (grid)
   (let* ((rows (length grid))
          (cols (length (first grid)))
          (new-size (+ rows cols))
          (rotated-grid (create-empty-grid new-size)))
     (dotimes (i rows)
       (dotimes (j cols)
         (let ((new-x ,new-x-expr)
               (new-y ,new-y-expr))
           ;; Ensure the indices are within bounds
           (when (and (>= new-x 0) (<= new-x new-size)
                      (>= new-y 0) (<= new-y new-size))
             (place-in-rotated-grid rotated-grid new-x new-y (cons (nth j (nth i  grid)) (cons i j)))))))
     (mapcan #'identity (concatenate-strings (mapcar (lambda (row) (remove nil row)) (2d-array-to-list rotated-grid)))))))

(define-rotate-grid rotate-grid-90
  j
  (- (1- rows) i)) 

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *grids0* (mapcar (lambda (str) (mapcar #'string (coerce str 'list))) *lines*))
(defparameter *grids1* '())

(dotimes (n (length *grids0*))
  (push (let ((grid '())) 
    (dotimes (m 7)
      (push (nth (+ n m) *grids0*) grid))
      (reverse grid)) *grids1*)
  (setf n (+ n 7)))

(setf *grids1* (reverse *grids1*))
(defparameter *grids* '())
(dolist (g *grids1*)
  (setf *grids* (append (list (rotate-grid-90 g)) *grids*)))

(defparameter *locks* (remove-if-not (lambda (item) (char-equal (char (nth 0 (nth 0 item)) 0) #\.)) *grids*))
(defparameter *keys* (remove-if (lambda (item) (char-equal (char (nth 0 (nth 0 item)) 0) #\.)) *grids*))

(defparameter *lockCodes0* (mapcar (lambda (item) (remove-if (lambda (s) (= 0 (length s))) (mapcar (lambda (item0) (nth 0 item0)) item))) *locks*))
(defparameter *lockCodes* (mapcar (lambda (item) (mapcar (lambda (item0) (1- (length (remove-if (lambda (s) (char-equal s #\.)) item0)))) item)) *lockCodes0*))

(defparameter *keyCodes0* (mapcar (lambda (item) (remove-if (lambda (s) (= 0 (length s))) (mapcar (lambda (item0) (nth 0 item0)) item))) *keys*))
(defparameter *keyCodes* (mapcar (lambda (item) (mapcar (lambda (item0) (1- (length (remove-if (lambda (s) (char-equal s #\.)) item0)))) item)) *keyCodes0*))

(defparameter *count* 0)
(dolist (l *lockCodes*)
  (dolist (k *keyCodes*)
    (setf fits T)
    (dotimes (n 5)
      (if (> (+ (nth n l) (nth n k)) 5)
        (setf fits nil)))
    (if fits
       (setf *count* (1+ *count*)))))

(format t "Count: ~a~%" *count*)