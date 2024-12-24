(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun split-line-to-pair (line)
  (split-sequence:split-sequence #\- line))

(defun flatten-one-level (lst)
  "Flatten a nested list by one level."
  (cond
    ((null lst) nil)  ;; Base case: empty list
    ((atom (car lst)) (cons (car lst) (flatten-one-level (cdr lst))))  ;; If the first element is an atom, add it to the result
    (t (append (car lst) (flatten-one-level (cdr lst))))))  ;; If the first element is a list, append its elements

(defun valid (item)
  (some (lambda (item0) (char-equal (char item0 0) #\t)) item))

(defun find2 (leftItem)
  (let ((leftLeft (first leftItem)) (leftRight (second leftItem)))
    (list leftItem
      (remove-duplicates (let ((items (remove-if (lambda (item)
        (or (string-equal leftleft item) (string-equal leftRight item)))
         
            (remove-if-not (lambda (right)
              (let ((rightLeft (first right)) (rightRight (second right)))
                (and
                  (not
                    (and
                      (string-equal leftleft rightleft)
                      (string-equal leftleft rightRight)
                      (string-equal leftRight rightleft)
                      (string-equal leftRight rightRight)))
                  (or
                    (string-equal leftleft rightleft)
                    (string-equal leftleft rightRight)
                    (string-equal leftRight rightleft)
                    (string-equal leftRight rightRight)))))
              *pairs*))))
        (remove-if (lambda (item)
          (=
            (1- (length items))
            (length
              (remove-if(lambda (item0)
                (string-equal item0 item))
              items))))
        items)) :test #'equal))))

(defun cross (item)
  (mapcar (lambda (item0) (sort (list (first (first item)) (second (first item)) item0) #'string<)) (second item)))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *pairs* (mapcar #'split-line-to-pair *lines*))
(defparameter *items* (remove-duplicates (flatten-one-level (mapcar #'cross (remove-if (lambda (item) (null (second item))) (mapcar #'find2 *pairs*)))) :test #'equal))
(defparameter *pairs0* (remove-if-not #'valid *items*))

(format t "Pairs: ~a~%" *pairs0*)
(format t "Length: ~a~%" (length *pairs0*))