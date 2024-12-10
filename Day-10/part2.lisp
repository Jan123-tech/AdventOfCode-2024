(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun string-to-string-list (str)
   (mapcar #'string (coerce str 'list)))

(defun convert-if-number (char)
  (if (every #'digit-char-p char)
      (parse-integer char)
      char))

(defun split-string-characters (str)
  (mapcar (lambda (char) (convert-if-number(string char))) (coerce str 'list)))

(defun lookup-char (x y grid)
    (nth y (nth x  grid)))

(defun lookup-point (point grid)
  (lookup-char (first point) (second point) grid))

(defun find-trail-starts (grid)
  (let ((items ()))
    (dotimes (i (length grid))
      (dotimes (j (length (first grid)))
        (let ((c (lookup-char i j grid)))
          (if (and (integerp c) (= c 0))
            (push (list i j) items)))))
    items))

(defmacro operate-on-pairs (op pair0 pair1)
  `(let ((x0 (first ,pair0)) (x1 (first ,pair1))
         (y0 (second ,pair0)) (y1 (second ,pair1)))
     (list (,op x0 x1) (,op y0 y1))))

(defun add (pair0 pair1)
  (operate-on-pairs + pair0 pair1))

(defun compare-char-number (c num)
  (and (integerp c) (= c num)))

(defun get-new-points (point)
  (remove-if-not (lambda (p)
    (let ((x (first p))
          (y (second p)))
      (and (>= x 0)
          (>= y 0)
          (< x (length *grid*))
          (< y (length (first *grid*)))))) (new-points point)))

(defun new-points (point)
  (mapcar (lambda (dir) (add point dir)) '(( -1 0) (0 1) (1 0) (0 -1))))

(defun remove-not-up (points num)
  (remove-if-not (lambda (point)
    (compare-char-number (lookup-point point *grid*) (1+ num))) points))

(defun increment-path (point visited)
    (let ((c (lookup-point point *grid*)))
      (setf (gethash point visited) t)
      (if (compare-char-number c 9)
        (list t)
        (let ((newPoints (remove-not-up (get-new-points point) c)))
          (if (= (length newPoints) 0)
            (list)
            (remove nil (mapcan (lambda (p) (increment-path p visited)) newPoints)))))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *grid* (mapcar #'split-string-characters *lines*))
(defparameter *starts* (find-trail-starts *grid*))

(reduce #'+ (mapcar (lambda (item) (length (increment-path item (make-hash-table :test 'equal)))) *starts*))