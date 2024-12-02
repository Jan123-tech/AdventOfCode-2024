(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defparameter *file-contents* (read-file "data.txt"))

(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))

(defun string-to-number-list (str)
    (mapcar #'parse-integer (split-sequence:split-sequence #\Space str)))

(defparameter *items0* (mapcar #'string-to-number-list *lines*))
(defparameter *items1* (mapcar (lambda (line) (list (butlast line) (rest line))) *items0*))
(defparameter *items2* (mapcar (lambda (item)
    (mapcar (lambda (x y) (cons x y))
        (first item)
        (second item)))
    *items1*))

(defparameter *validDiffs0* (remove-if-not
    (lambda (pairs)
        (every (lambda (pair)
            (let ((diff (abs (- (first pair) (cdr pair)))))
                (and (<= diff 3) (> diff 0))))
        pairs))
    *items2*))

(defparameter *validDiffs1* (remove-if-not
    (lambda (pairs)
        (let ((diffs (mapcar (lambda (pair) (- (first pair) (cdr pair))) pairs)))
            (or
                (every (lambda (diff) (< diff 0)) diffs)
                (every (lambda (diff) (> diff 0)) diffs))))
    *validDiffs0*))

(format t "~A~%" (length *validDiffs1*)))