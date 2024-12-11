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

(defun store-future-value (key value)
  (setf (gethash key *futures-values*) value))

(defun get-future-value (key)
  (gethash key *futures-values* nil))

(defmacro define-iterate (func-name recursive-func)
  `(defun ,func-name (num height)
     (let ((newNums (transform num)))
       (let ((result (if (= height 1)
                         (length newNums)
                         (reduce #'+ (remove nil (mapcar (lambda (n)
                                                          (,recursive-func n (1- height)))
                                                        newNums))))))
         (store-future-value (list num height) result)
         result))))

(defun iterate-cached (num height)
 (if (<= height *cacheHeight*)
    (progn
      (let ((cachedValue (get-future-value (list num height))))
        (if cachedValue
          cachedValue
          (iterate-inner-cached num height))))
      (progn
        (iterate-inner-cached num height)
      )))

(define-iterate iterate iterate)
(define-iterate iterate-inner-cached iterate-cached)

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *numbers* (split-string-into-integers *file-contents*))
(defparameter *cacheHeight* 40)

(time (progn

  (defparameter *futures-values* (make-hash-table :test 'equal))
  (dolist (num *numbers*) (iterate num *cacheHeight*))
  (format t "Cache Count: ~A~%" (hash-table-count *futures-values*))

  (defparameter *mainSum* (reduce #'+ (mapcar (lambda (n) (iterate-cached n 75)) *numbers*)))

  (format t "Count: ~A~%" (+ *mainSum*))))