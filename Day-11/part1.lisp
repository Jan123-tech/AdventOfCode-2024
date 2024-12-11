(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun split-string-into-integers (input-string)
  (let ((substrings (split-sequence:split-sequence #\Space input-string)))
    (mapcar #'parse-integer substrings)))

(defun split-integer (number)
  (let ((num-str (write-to-string number)))
    (if (evenp (length num-str)) 
        (let* ((half-length (/ (length num-str) 2))
               (first-half (subseq num-str 0 half-length))
               (second-half (subseq num-str half-length)))
          (list (parse-integer first-half) (parse-integer second-half)))
        nil)))

(defun transform (num)
  (if (= num 0)
    '(1)
    (let ((split (split-integer num)))
      (if split
        split
        (list (* num 2024))))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *numbers* (split-string-into-integers *file-contents*))

(defparameter *final*
  (let ((nums *numbers*))
    (dotimes (i 25)
      (let ((newNums ()))
        (dolist (num nums)
          (setf newNums (append newNums (transform num))))
        (setf nums newNums)))
      nums))

(format t "Count: ~A~%" (length *final*)))