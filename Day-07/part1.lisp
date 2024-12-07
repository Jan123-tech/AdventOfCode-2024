(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun split-line-to-pair (line)
  (let ((parts (split-sequence:split-sequence #\: line)))
    (cons (parse-integer (first parts)) (string-to-number-list (second parts)))))

(defun string-to-number-list (str)
    (list (mapcar #'parse-integer (split-sequence:split-sequence #\Space (string-trim " " str)))))

(defun number-to-binary-list (num)
  (let ((bits ()))
    (dotimes (i (integer-length num))
      (push (logand 1 (ash num (- i))) bits))
    (reverse bits)))

(defun getBits (num)
  (let ((nums ()))
    (dotimes (i (expt 2 num))
      (let ((bits (number-to-binary-list i)))
        (dotimes (j (- num (length bits)))
          (if (= (length bits) 0)
            (setf bits '(0))
            (setf bits (append bits '(0)))))
        (push bits nums)))
  (reverse nums)))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *pairs* (mapcar #'split-line-to-pair *lines*))

(defparameter *results* (mapcar (lambda (item)
  (cons (first item)
    (list (let* ((items (second item)) (combos (getBits (- (length items) 1))))
      (mapcar (lambda (combo)
        (let ((index -1))
          (reduce (lambda (acc num)
            (progn
              (setf index (+ index 1))
              (if (= (nth index combo) 1) (+ acc num) (* acc num)))) (rest items) :initial-value (first items))))
        combos)))))
  *pairs*))

(defparameter *valid* (remove-if-not (lambda (item)
    (some (lambda (acc) (= acc (first item))) (second item)))
  *results*))

(format t "~A~%" (reduce #'+ (mapcar #'first *valid*)))