(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

; manually removed two spaces from the delimiter
(defparameter *file-contents* (read-file "aoc1.csv"))

(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))

(defun split-line-to-pair (line)
  (let ((parts (split-sequence:split-sequence #\Space line)))
    (cons (parse-integer(first parts)) (parse-integer(second parts)))))

(defparameter *pairs* (mapcar #'split-line-to-pair *lines*))

(defparameter *items0* (sort (mapcar #'first *pairs*) #'<))
(defparameter *items1* (sort (mapcar #'rest *pairs*) #'<))
(defparameter *items2* (mapcar #'cons *items0* *items1*))
(defparameter *items3* (mapcar (lambda (pair) (abs (- (first pair) (rest pair)))) *items2*))
(defparameter *sum* (reduce #'+ *items3*))

(format t "~A~%" *sum*)















