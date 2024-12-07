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

(defun set-char-in-grid (pos char grid)
  (setf (nth (cdr pos) (nth (first pos) grid)) char))

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

(defun make-key (int pair)
  (list int pair))

(defun move (pos directionIndex path grid) {
  (block find-start
    (let ((newPos (new-position pos (get-direction directionIndex))))
      (if (not (null (gethash (make-key directionIndex newPos) path)))
        (progn
          (defparameter *numberOfLoops* (+ *numberOfLoops* 1))
          (return-from find-start path))
        (if (or
            (< (first newPos) 0)
            (>= (first newPos) (length grid))
            (< (cdr newPos) 0)
            (>= (cdr newPos) (length (first grid))))
          (return-from find-start path)
          (let ((newC (lookup-char newPos grid)))
            (if (string-equal newC "#")
              (move pos (get-next-direction-index directionIndex) path grid)
              (progn
                (setf (gethash (make-key directionIndex newPos) path) t)
                (move newPos directionIndex path grid)))))))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *grid* (mapcar #'string-to-string-list *lines*))

(defparameter *numberOfLoops* 0)
(defparameter *startPos* (find-start *grid*))
(defparameter *startDirectionIndex* 0)

(dotimes (i (length *grid*))
  (dotimes (j (length (first *grid*)))
    (let ((path (make-hash-table :test 'equal)) (c (lookup-char (cons i j) *grid*)))
      (if (not (string-equal c "#"))
        (progn
          (set-char-in-grid (cons i j) "#" *grid*)
          (setf (gethash (make-key *startDirectionIndex* *startPos*) path) t)
          (move *startPos* *startDirectionIndex* path *grid*)
          (set-char-in-grid (cons i j) "." *grid*))))))

(format t "Loops: ~a~%" *numberOfLoops*)