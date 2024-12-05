(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun split-to-integer-pairs (string-list)
  (mapcar (lambda (str)
            (let ((parts (split-sequence:split-sequence #\| str)))
              (cons (parse-integer (first parts))
                    (parse-integer (second parts)))))
          string-list))

(defun add-to-dictionary (pairs)
  (let ((dict (make-hash-table)))
    (dolist (pair pairs)
      (let ((key (car pair))
            (value (cdr pair)))
        (unless (gethash key dict)
          (setf (gethash key dict) (make-hash-table)))
        (setf (gethash value (gethash key dict)) t)))
    dict))

(defun parse-integer-lists (string-list)
  (mapcar (lambda (str)
            (mapcar #'parse-integer
                    (split-sequence:split-sequence #\, str)))
          string-list))

(defun validate (num nums)
  (if (null nums)
      t
      (if (every (lambda (n)
            (let ((dict (gethash n *dict*)))
              (if dict
                (null (gethash num dict))
                t)))
          nums)
        (validate (first nums) (rest nums))
        nil)))

(defun print-hash-table (hash-table)
  (maphash (lambda (key value)
             (if (hash-table-p value)
                 (format t "Key: ~a, Values: ~{~a~^, ~}~%"
                         key
                         (loop for k being the hash-keys of value
                               collect k))
                 (format t "Key: ~a, Value is not a hash table: ~a~%" key value)))
           hash-table))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *integer-pairs* (split-to-integer-pairs *lines*))
(defparameter *dict* (add-to-dictionary *integer-pairs*))

(defparameter *file-contents2* (read-file "data2.txt"))
(defparameter *lines2* (split-sequence:split-sequence #\Newline *file-contents2*))
(defparameter *items* (parse-integer-lists *lines2*))

(defparameter *valid* (remove-if-not (lambda (item) (validate (first item) (rest item))) *items*))
(defparameter *sum* (reduce #'+ (mapcar (lambda (item) (nth (/ (- (length item) 1) 2) item)) *valid*)))