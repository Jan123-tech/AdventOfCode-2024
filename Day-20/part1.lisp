(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun string-to-string-list (str)
  (mapcar #'string (coerce str 'list)))

(defun lookup-char (pos grid)
  (nth (second pos) (nth (first pos) grid)))

(defun get-items (str)
  (let* ((items (make-hash-table :test 'equal))
    (lines (split-sequence:split-sequence #\Newline str)) (grid (mapcar #'string-to-string-list lines)))
      (defparameter *width* (length (first grid)))
      (defparameter *height* (length grid))
      (dotimes (i *height*)
        (dotimes (j *width*)
          (let ((c (lookup-char (list i j) grid)))
            (if (string-equal c "S")
              (progn
                (defparameter *start* (list i j)))
              (if (string-equal c "E")
                (progn
                  (defparameter *end* (list i j)))
                (if (string-equal c "#")
                  (setf (gethash (list i j) items) c)))))))
  items))

(defun new-position (pos direction)
  (list (+ (first pos) (first direction)) (+ (second pos) (second direction))))

(defun get-new-positions (pos)
  (mapcar (lambda (item) (list (new-position pos item) item)) (get-directions)))

(defun get-directions ()
  (list (list -1 0) (list 0 1) (list 1 0) (list 0 -1)))

(defun print-hash-table (hash-table)
  (maphash (lambda (key value)
             (if (hash-table-p value)
                 (format t "Key: ~a, Values: ~{~a~^, ~}~%"
                         key
                         (loop for k being the hash-keys of value
                               collect k))
                 (format t "Key: ~a, Value: ~a~%" key value)))
           hash-table))

(defun convert-queue-to-hash-table (queue)
  "Convert the queue to a hash table by iterating along the chain and gathering points."
  (let ((result (make-hash-table :test 'equal)))
    (labels ((process-item (item)
               (let ((point (first item))
                     (parent (third item)))
                 ;; Add the point and its parent to the hash table
                 (setf (gethash point result) parent)
                 ;; If the parent is not nil, process the parent
                 (when parent
                   (process-item parent)))))
      ;; Iterate through the queue and process each item
      (dolist (item queue)
        (process-item item)))
    result))

(defun render-grid-with-start-end-and-direction (points start-point end-point more-points width height more-points-char)
  "Render a grid with the given points, start point, end point, direction points, width, and height."
  (let ((grid (make-array (list height width) :initial-element #\.)))
    ;; Populate the grid with points
    (maphash (lambda (key value)
               (setf (aref grid (first key) (second key)) #\#))
             points)
    ;; Populate the grid with direction points based on keys
    (maphash (lambda (key value)
               (setf (aref grid (first key) (second key)) more-points-char))
             more-points)
    ;; Set the start point and end point
    (setf (aref grid (first start-point) (second start-point)) #\S)
    (setf (aref grid (first end-point) (second end-point)) #\E)
    ;; Print the grid
    (dotimes (i height)
      (dotimes (j width)
        (princ (aref grid i j)))
      (terpri))))

(defun count-items-in-chain (endPath)
  "Count the number of items in the chain ending at endPath."
  (let ((count 0)
        (current endPath))
    (loop
      (incf count)
      (setf current (second current))
      (unless current
        (return (1- count))))))

(defun move-min ()
  (if (= (length *queue*) 0)
    nil
    (let* ((item (pop *queue*)) (point (first item)))
      ;(format t "Item: ~a~%" item)
      (if (and (= (first point) (first *end*)) (= (second point) (second *end*)))
        (progn
          ;(format t "Finished")
          (defparameter *endPath* item)
          nil)
        (let* ((newPositions (get-new-positions (first item)))
          (filteredNewPositions (remove-if-not (lambda (p)
            (and
              (null (gethash (first p) *visisted*))
              (or
                (null (gethash (first p) *items*))
                (and
                  (equal (first p) (first *cheat*))
                  (equal (second p) (second *cheat*)))))) newPositions)))
          ;(format t "New positions: ~a~%" filteredNewPositions)
          (dolist (newPos filteredNewPositions)
            (let ((point (first newPos)) (direction (second newPos)))
              (setf (gethash point *visisted*) t)
              (setf *queue* (append *queue* (list (list point item)))))
          )
          (move-min)))
    )))

(defun item-with-minimum-score (items)
  (reduce (lambda (item1 item2)
            (if (< (count-items-in-chain item1) (count-items-in-chain item2))
                item1
                item2))
          items))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *items* (get-items *file-contents*))
(defparameter *cheat* '((-1 -1) (0 0)))
(defparameter *noCheatCount* (progn
  (defparameter *visisted* (make-hash-table :test 'equal))
  (defparameter *queue* (list (list *start* nil)))
  (defparameter *endPath* nil)
  (move-min)
  (count-items-in-chain *endPath*)))

(format t "No-cheat count: ~a~%" *noCheatCount*)

(defparameter *times* '())

(dotimes (i (1- *width*))
  (format t "Column: ~a~%" i)
  (dotimes (j (1- *height*))
    (if (and (not (null (gethash (list j i) *items*))) (> i 0) (> j 0))
      (dolist (c (remove-if (lambda (item) (gethash (first item) *items*)) (get-new-positions (list j i))))
        (defparameter *visisted* (make-hash-table :test 'equal))
        (defparameter *queue* (list (list *start* nil)))
        (defparameter *endPath* nil)
        (defparameter *cheat* (list (list j i) (second c)))
        (move-min)
        (push (count-items-in-chain *endPath*) *times*)))))

(defparameter *savings* (remove-if-not (lambda (saving) (>= saving 100)) (mapcar (lambda (item) (- *noCheatCount* item)) *times*)))

(format t "Savings: ~a~%" (length *savings*))

(render-grid-with-start-end-and-direction *items* *start* *end* (convert-queue-to-hash-table (list *endPath*)) *width* *height* ">")