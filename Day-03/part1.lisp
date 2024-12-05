(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun string-to-string-list (str)
   (mapcar #'string (coerce str 'list)))

(defun list-of-lists-to-2d-array (list-of-lists)
  (let* ((rows (length list-of-lists))
         (cols (length (first list-of-lists)))
         (array (make-array (list rows cols) :initial-element "")))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf (aref array i j) (nth j (nth i list-of-lists)))))
    array))

(defun lookup-char (x y grid)
    (nth y (nth x  grid)))

(defun transpose-grid (grid)
  (apply #'mapcar (lambda (&rest rows) rows) grid))
  
(defun concatenate-strings (string-list)
  (apply #'concatenate 'string string-list))

(defun copy-and-flatten-grid (grid)
  (let* ((horizontal-copies (copy-seq (mapcar #'concatenate-strings grid)))
         (transposed-grid (transpose-grid grid))
         (vertical-copies (copy-seq (mapcar #'concatenate-strings transposed-grid))))
   (append horizontal-copies vertical-copies)))

(defun create-empty-grid (size)
  (make-array (list size size) :initial-element nil))

(defun place-in-rotated-grid (grid x y element)
  (setf (aref grid x y) element))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

(defun concatenate-strings2 (nested-lists)
  (mapcar (lambda (inner-list)
            (list (apply #'concatenate 'string inner-list)))
          nested-lists))

(defun concatenate-inner-lists (nested-lists)
  (mapcar (lambda (inner-lists)
            (mapcar (lambda (strings)
                      (apply #'concatenate 'string (map #'identity strings)))
                    inner-lists))
          nested-lists))

(defun rotate-grid-45 (grid)
  (let* ((rows (length grid))
         (cols (length (first grid)))
         (new-size (+ rows cols -1))
         (rotated-grid (create-empty-grid new-size)))
    (dotimes (i rows)
      (dotimes (j cols)
        (let ((new-x (+ i j))
              (new-y (- (+ cols i) j)))
          ;; Ensure the indices are within bounds
          (when (and (>= new-x 0) (< new-x new-size)
                     (>= new-y 0) (< new-y new-size))
            (place-in-rotated-grid rotated-grid new-x new-y (lookup-char i j grid))))))
    (flatten (concatenate-strings2 (mapcar (lambda (row) (remove nil row)) (2d-array-to-list rotated-grid))))))

(defun rotate-grid-45-reverse (grid)
  (let* ((rows (length grid))
         (cols (length (first grid)))
         (new-size (+ rows cols -1))
         (rotated-grid (create-empty-grid new-size)))
    (dotimes (i rows)
      (dotimes (j cols)
(let ((new-x (+ j (- rows i 1)))
              (new-y (+ i j)))
          ;; Ensure the indices are within bounds
          (when (and (>= new-x 0) (< new-x new-size)
                     (>= new-y 0) (< new-y new-size))
            (place-in-rotated-grid rotated-grid new-x new-y (lookup-char i j grid))))))
    (flatten (concatenate-strings2 (mapcar (lambda (row) (remove nil row)) (2d-array-to-list rotated-grid))))))

(defun reverse-string (str)
  (let ((char-list (coerce str 'list)))
    (coerce (reverse char-list) 'string)))

(defun reverse-strings (strings)
    (mapcar (lambda (s0) (reverse s0)) strings))

(defun count-substring-occurrences (string substring)
  (let ((count 0)
        (start 0))
    (loop
       (let ((pos (search substring string :start2 start)))
         (if pos
             (progn
               (incf count)
               (setf start (+ pos (length substring))))
             (return count))))))

(defun count-occurrences-in-list (strings substring)
  (reduce #'+ (mapcar (lambda (str) (count-substring-occurrences str substring)) strings)))

(defun flatten (nested-list)
  (if (atom nested-list)
      (list nested-list)
      (apply #'append (mapcar #'flatten nested-list))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *grid* (mapcar #'string-to-string-list *lines*))
(defparameter *flat-list* (copy-and-flatten-grid *grid*))
(defparameter *rotated-grid*  (rotate-grid-45 *grid*))
(defparameter *rotated-grid-reverse* (rotate-grid-45-reverse *grid*))

(defparameter *joined-list* (append *flat-list* *rotated-grid* *rotated-grid-reverse*))
(defparameter *final-strings* (append *joined-list* (reverse-strings *joined-list*)))

(defparameter *occurrences* (count-occurrences-in-list *final-strings* "XMAS"))

(format t "~a~%" *occurrences*)