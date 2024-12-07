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

(defun number-to-base3-list (num)
  (let ((digits ()))
    (loop while (> num 0)
          do (push (mod num 3) digits)
          (setf num (floor num 3)))
    (if (null digits) (push 0 digits))
    (reverse digits)))

(defun getCombos (num)
  (let ((nums ()))
    (dotimes (i (expt 3 num))
      (let ((bits (number-to-base3-list i)))
          (push bits nums)))
    nums))

(defun concatenate-numbers (num1 num2)
  (let* ((str1 (write-to-string num1))
         (str2 (write-to-string num2))
         (concatenated-str (concatenate 'string str1 str2)))
    (parse-integer concatenated-str)))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *pairs* (mapcar #'split-line-to-pair *lines*))

(defparameter *results* (mapcar (lambda (item)
  (cons (first item)
    (list (let* ((items (second item)) (combos (getCombos (- (length items) 1))))
      (mapcar (lambda (combo)
        (let ((index -1))
          (reduce (lambda (acc num)
            (progn
              (setf index (+ index 1))
              (let ((op (nth index combo)))
                (if (or (null op) (= op 0)) (concatenate-numbers acc num)
                  (if (= op 1) (* acc num)
                    (+ acc num))))))
            (rest items) :initial-value (first items))))
        combos)))))
  *pairs*))

(defparameter *valid* (remove-if-not (lambda (item)
    (some (lambda (acc) (= acc (first item))) (second item)))
  *results*))

(format t "~A~%" (reduce #'+ (mapcar #'first *valid*)))