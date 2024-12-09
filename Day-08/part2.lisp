(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun string-to-string-list (str)
   (mapcar #'string (coerce str 'list)))

(defun lookup-char (x y grid)
    (nth y (nth x  grid)))

(defun find-antennas (grid)
  (let ((items ()))
    (dotimes (i (length grid))
      (dotimes (j (length (first grid)))
        (let ((c (lookup-char i j grid)))
          (if (not (string-equal c "."))
            (push (cons c (cons i j)) items)))))
    items))

(defun group-antennas (items)
  (let ((dict (make-hash-table :test 'equal)))
    (dolist (item items)
      (let ((c (first item)))
        (unless (gethash c dict)
          (setf (gethash c dict) ()))
        (setf (gethash c dict) (push (cdr item) (gethash c dict)))))
     dict))

(defun hash-table-to-list-of-lists (hash-table)
    (let ((keys ())) (maphash (lambda (key value) (push (cons key (list value)) keys)) hash-table) keys))

(defun getPairs (items)
  (let ((pairs ()) (head (first items)) (rest (cdr items)))
    (dolist (item rest)
      (push (cons head (list item)) pairs))
    (append pairs (if (> (length rest) 1) (getPairs rest) '()))))

(defmacro operate-on-pairs (op pair0 pair1)
  `(let ((x0 (first ,pair0)) (x1 (first ,pair1))
         (y0 (cdr ,pair0)) (y1 (cdr ,pair1)))
     (cons (,op x0 x1) (,op y0 y1))))

(defun add (pair0 pair1)
  (operate-on-pairs + pair0 pair1))

(defun sub (pair0 pair1)
  (operate-on-pairs - pair0 pair1))

(defun generate-points (count op point diff)
  (if (= count 0)
    nil
    (let ((newPoint (funcall op point diff)))
      (append (list newPoint) (generate-points (- count 1) op newPoint diff)))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *grid* (mapcar #'string-to-string-list *lines*))
(defparameter *groups* (hash-table-to-list-of-lists (group-antennas (find-antennas *grid*))))
(defparameter *combos* (mapcar (lambda (item) (cons (first item) (getPairs (second item)))) *groups*))

(defparameter *locations* (let ((dict (make-hash-table :test 'equal)))
  (dolist (item *combos*)
      (dolist (pair (cdr item))
        (let* ((pos0 (first pair)) (pos1 (second pair)) (diff (sub pos0 pos1)) (numPoints 30))
          (dolist (p (append (generate-points numPoints #'add pos1 diff) (generate-points numPoints #'sub pos0 diff)))
            (let ((x (first p)) (y (cdr p)))
              (if (and
                (>= x 0)
                (< x (length *grid*))
                (>= y 0)
                (< y (length (first *grid*))))
                  (unless (gethash p dict)
                    (setf (gethash p dict) t))))))))
    dict))

(format t "~A~%" *locations*)