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

(defun remove-one-item (lst)
  (loop for i from 0 below (length lst)
        collect (append (subseq lst 0 i) (subseq lst (1+ i)))))

(defparameter *itemsItems0* (mapcar (lambda (line) (remove-one-item line)) *items0*))

(defparameter *itemsItems1* (mapcar
        (lambda (items)
            (mapcar (lambda (line) (list (butlast line) (rest line))) items))
    *itemsItems0*))

(defparameter *itemsItems2* (mapcar
        (lambda (items)
            (mapcar (lambda (item)
                (mapcar (lambda (x y) (cons x y))
                    (first item)
                    (second item)))
            items))
    *itemsItems1*))

(defparameter *itemsItems3* (mapcar
        (lambda (items)
            (remove-if-not
                (lambda (pairs)
                    (and
                        (every (lambda (pair)
                            (let ((diff (abs (- (first pair) (cdr pair)))))
                                (and (<= diff 3) (>= diff 1))))
                        pairs)

                        (let ((diffs (mapcar (lambda (pair) (- (first pair) (cdr pair))) pairs)))
                            (or
                                (every (lambda (diff) (< diff 0)) diffs)
                                (every (lambda (diff) (> diff 0)) diffs)))))
                items))
    *itemsItems2* ))

(defparameter *itemsItems4* (remove-if-not (lambda (items) (> (length items) 0)) *itemsItems3*))

(format t "~A~%" (length *itemsItems4*))