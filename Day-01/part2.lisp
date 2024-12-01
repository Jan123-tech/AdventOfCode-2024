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

(defparameter *items0* (mapcar #'first *pairs*))
(defparameter *items1* (mapcar #'rest *pairs*))
(defparameter *items2* (remove-if-not (lambda (item) (member item *items0*)) *items1*))

(defparameter *counts*
  (let ((counts (make-hash-table)))
    (dolist (item *items2*)
      (incf (gethash item counts 0)))
    counts))

(defparameter *items3*
  (let ((products '()))
    (maphash (lambda (key value) (push (* key value) products)) *counts*)
    products))

(defparameter *sum* (reduce #'+ *items3*))

(format t "~A~%" *sum*)