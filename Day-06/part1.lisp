(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun string-to-string-list (str)
  (mapcar #'string (coerce str 'list)))

(defun lookup-char (pos grid)
  (nth (cdr pos) (nth (first pos) grid)))

(defun find-start (grid)
  (block find-start
    (dotimes (i (length grid))
      (dotimes (j (length (first grid)))
        (let ((c (lookup-char (cons i j) grid)))
          (if (string-equal c "^")
            (return-from find-start (cons i j))))))))

(defun get-next-direction-index (index)
  (if (= (+ index 1) 4) 0 (+ index 1)))

(defun get-direction (index)
  (nth index (list (cons -1 0) (cons 0 1) (cons 1 0) (cons 0 -1))))

(defun new-position (pos direction)
  (cons (+ (first pos) (first direction)) (+ (cdr pos) (cdr direction))))

(defun move (pos directionIndex path) {
  (block find-start
    (let ((newPos (new-position pos (get-direction directionIndex))))
      (if (or
          (< (first newPos) 0)
          (>= (first newPos) (length *grid*))
          (< (cdr newPos) 0)
          (>= (cdr newPos) (length (first *grid*))))
        (return-from find-start path)
        (let ((newC (lookup-char newPos *grid*)))
          (if (string-equal newC "#")
            (move pos (get-next-direction-index directionIndex) path)
            (progn
              (push newPos path)
              (move newPos directionIndex path))))))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *grid* (mapcar #'string-to-string-list *lines*))

(defparameter *path* (move (find-start *grid*) 0 (list))))

(format t "~a~%" (length (remove-duplicates *path* :test #'equal)))