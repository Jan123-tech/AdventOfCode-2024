(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

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

(define-rotate-grid rotate-grid-45
  (+ i j)
  (- (+ cols i) j))

(define-rotate-grid rotate-grid-45-reverse
  (+ j (- rows i 1))
  (+ i j))

(defun reverse-strings (strings)
    (mapcar (lambda (s0)
        (cons
            (reverse (first s0))
            (list (reverse (second s0)))))
    strings))
  
(defun count-substring-occurrences (string substring)
  (let ((xy (list)) (start 0) (count 0))
    (loop
       (let ((pos (search substring (first string) :start2 start)))
         (if pos
             (progn
               (push (nth (+ pos 1) (second string)) xy)
               (setf start (+ pos (length substring))))
             (return (cons string (list xy))))))))

(defun count-occurrences-in-list (strings substring)
  (reduce #'append
    (mapcar #'second
      (remove-if-not (lambda (item) (second item)) (mapcar (lambda (str) (count-substring-occurrences str substring)) strings)))))

(defun count-common-points (list1 list2)
  (let ((count 0))
    (dolist (point list1 count)
      (when (member point list2 :test #'equal)
        (incf count)))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *grid* (mapcar (lambda (str) (mapcar #'string (coerce str 'list))) *lines*))

(defparameter *rotated-grid-45* (rotate-grid-45 *grid*))
(defparameter *rotated-grid-45-reverse* (reverse-strings *rotated-grid-45*))
(defparameter *rotated-grid-45-all* (append *rotated-grid-45* *rotated-grid-45-reverse*))

(defparameter *rotated-grid-45-neg* (rotate-grid-45-reverse *grid*))
(defparameter *rotated-grid-45-neg-reverse* (reverse-strings *rotated-grid-45-neg*))
(defparameter *rotated-grid-45-neg-all* (append *rotated-grid-45-neg* *rotated-grid-45-neg-reverse*))

(defparameter *occurrences-45-points* (count-occurrences-in-list *rotated-grid-45-all* "MAS"))
(defparameter *occurrences-45-neg-points* (count-occurrences-in-list *rotated-grid-45-neg-all* "MAS"))

(defparameter *common* (count-common-points *occurrences-45-points* *occurrences-45-neg-points*))

(format t "~a~%" *common*)