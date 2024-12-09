(ql:quickload "split-sequence")

(use-package :split-sequence)

(defstruct d
  exp
  orig)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun split-string-characters (str)
  (mapcar (lambda (char) (parse-integer(string char))) (coerce str 'list)))

(defun iterate-pairs (list)
  (loop for i from 0 below (length list) by 2
        collect (list (nth i list)
                      (if (< (+ i 1) (length list))
                          (nth (+ i 1) list)
                          0))))

(defun generate-disk-items (num expanded-index orig-index)
  (if (= num 0)
    nil
    (append (list (make-d :exp expanded-index :orig orig-index))
      (generate-disk-items (1- num) (1+ expanded-index) orig-index))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *characters* (split-string-characters *file-contents*))

(defparameter *empties-queue* '())
(defparameter *file-stack* '())
(defparameter *file-moved* '())

(let ((orig-index 0) (expanded-index 0))
    (dolist (pair (iterate-pairs *characters*))
      (let* ((fileCount (first pair)) (emptyCount (second pair)) (emptiesStartIndex (+ expanded-index fileCount)))
        (dolist (item (generate-disk-items fileCount expanded-index orig-index))
          (push item *file-stack*))
        (dolist (item (generate-disk-items emptyCount emptiesStartIndex orig-index))
          (setf *empties-queue* (append *empties-queue* (list item))))
        (setf expanded-index (+ emptiesStartIndex emptyCount))
        (incf orig-index))))

(loop while (> (d-exp (first *file-stack*)) (d-exp (first *empties-queue*)))
  do
    (setf program (pop *file-stack*))
    (setf empty (pop *empties-queue*))
    (push (make-d :exp (d-exp empty) :orig (d-orig program)) *file-moved*)
    (setf *empties-queue* (append *empties-queue* (list (make-d :exp (d-exp program) :orig (d-orig empty)))))
)

(defparameter *file-all* (append *file-stack* *file-moved*))
(defparameter *sum* (reduce #'+ (mapcar (lambda (item) (* (d-exp item) (d-orig item))) *file-all*)))

(format t "~A~%" *sum*)