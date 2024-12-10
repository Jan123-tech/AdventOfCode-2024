(ql:quickload "split-sequence")

(use-package :split-sequence)

(defstruct d
  exp
  orig
  isFile)

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

(defun generate-disk-items (num expanded-index orig-index isFile)
  (if (= num 0)
    nil
    (append (list (make-d :exp expanded-index :orig orig-index :isFile isFile))
      (generate-disk-items (1- num) (1+ expanded-index) orig-index isFile))))

(defun find-list-index-and-item (list-of-lists n exp)
  (loop for sublist in list-of-lists
        for index from 0
        when (and (>= (length sublist) n)
                  (< (d-exp (first sublist)) exp))
        return (list index sublist)))

(defun insert-sublist-at-index (list-of-lists new-sublist index)
  (concatenate 'list
               (subseq list-of-lists 0 index)
               (list new-sublist)
               (subseq list-of-lists index)))

(defun replace-sublist-at-index (list-of-lists index new-sublist)
  (concatenate 'list
               (subseq list-of-lists 0 index) 
               (list new-sublist)
               (subseq list-of-lists (1+ index))))

(defun remove-sublist-at-index (list-of-lists index)
  (concatenate 'list
               (subseq list-of-lists 0 index)
               (subseq list-of-lists (1+ index))))

(defun find-insertion-index (list-of-sublists lowest-exp)
  (let ((index 0))
    (loop for sublist in list-of-sublists
          while (> lowest-exp (d-exp (first sublist)))
          do (incf index))
    index))

(defun first-or-cdr (sublist)
  (if (= (length sublist) 1)
      (first sublist)
      (car (cdr sublist))))

(defun last-n-items (list n)
  (subseq list (max 0 (- (length list) n))))

(defun format-d-list (d-list)
  (let ((sorted-list (sort (copy-list d-list) #'< :key #'d-exp)))
    (loop for item in sorted-list
          do (if (not (d-isFile item))
                 (format t ".")
                 (format t "~a" (d-orig item))))
    (terpri)))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *characters* (split-string-characters *file-contents*))

(defparameter *empties* '())
(defparameter *file-stack* '())
(defparameter *file-processed* '())

(let ((orig-index 0) (expanded-index 0))
    (dolist (pair (iterate-pairs *characters*))
      (let* ((fileCount (first pair)) (emptyCount (second pair)) (emptiesStartIndex (+ expanded-index fileCount)))
        (push (generate-disk-items fileCount expanded-index orig-index t) *file-stack*)
        (setf *empties* (append *empties* (remove nil (list (generate-disk-items emptyCount emptiesStartIndex orig-index nil)))))
        (setf expanded-index (+ emptiesStartIndex emptyCount))
        (incf orig-index))))

(dolist (file *file-stack*)
    (let* ((emptyBlockAndIndex (find-list-index-and-item *empties* (length file) (d-exp (first file))))
       (index (first emptyBlockAndIndex)) (emptyBlock (second emptyBlockAndIndex)))
       (pop *file-stack*)
      (if emptyBlock
        (let ((fileLength (length file)) (emptyBlockLength (length emptyBlock)))
          (if (= fileLength emptyBlockLength)
            (setf *empties* (remove-sublist-at-index *empties* index))
            (setf *empties* (replace-sublist-at-index *empties* index (last-n-items emptyBlock (- emptyBlockLength fileLength)))))

            (let ((fileExp (d-exp (first file))) (newEmpties '()))

              (defparameter *insertion-index* (find-insertion-index *empties* fileExp))
              (if (> *insertion-index* 0)
                (let ((prevIndex (1- *insertion-index*)))
                  (defparameter *before* (nth prevIndex *empties*))
                  (if (= (1- fileExp) (d-exp (first-or-cdr *before*)))
                    (progn
                      (setf *empties* (remove-sublist-at-index *empties* prevIndex))
                      (setf newEmpties (append newEmpties *before*))
                      (defparameter *insertion-index* (1- *insertion-index*)))
                    nil))
                nil)

              (setf newEmpties (append newEmpties (generate-disk-items fileLength fileExp (d-orig (first file)) nil)))

              (if (< *insertion-index* (1- (length *empties*)))
                (let ((nextIndex (1+ *insertion-index*)))
                  (defparameter *after* (nth nextIndex *empties*))
                  (if (= (1+ fileExp) (d-exp (first *after*)))
                    (progn
                      (setf *empties* (remove-sublist-at-index *empties* nextIndex))
                      (setf newEmpties (append newEmpties *after*)))
                    nil))
                nil)
              (setf *empties* (insert-sublist-at-index *empties* newEmpties *insertion-index*))
            )
          (setf file (generate-disk-items fileLength (d-exp (first emptyBlock)) (d-orig (first file)) t))))
      (push file *file-processed*)
    ))

(defparameter *sum* (reduce #'+ (mapcar (lambda (item) (* (d-exp item) (d-orig item))) (apply #'append *file-processed*))))

(format t "~A~%" *sum*)