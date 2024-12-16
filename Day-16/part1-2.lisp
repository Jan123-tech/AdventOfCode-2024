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
  (mapcar (lambda (item) (list (new-position pos item) item)) (list (list -1 0) (list 0 1) (list 1 0) (list 0 -1))))

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

(defun get-change-increment (dir newDir)
  (if (and (= (first dir) (first newDir)) (= (second dir) (second newDir)))
    0
    1))

(defun count-items-in-chain (endPath)
  "Count the number of items in the chain ending at endPath."
  (let ((count 0)
        (current endPath))
    (loop
      (incf count)
      (setf current (third current))
      (unless current
        (return count)))))

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
          (filteredNewPositions (remove-if-not (lambda (p) (and (null (gethash (first p) *visisted*)) (null (gethash (first p) *items*)))) newPositions)))
          ;(format t "New positions: ~a~%" filteredNewPositions)
          (dolist (newPos filteredNewPositions)
            (let ((point (first newPos)) (direction (second newPos)))
              (setf (gethash point *visisted*) t)
              (setf *queue* (append *queue* (list (list point direction item (+ (fourth item) (get-change-increment (second item) direction)))))))
          )
          (move-min)))
    )))

(defun move (item)
  (let* ((point (first item)))
    ;(format t "Item: ~a~%" item)
    (multiple-value-bind (value found) (gethash (list point (second item)) *visitedFailed*)
      (if (and found (>= (fourth item) value))
          (progn
           ; (format t "Bad Path ~a with value ~a~%" item value)
            nil)
          (if (and (= (first point) (first *end*)) (= (second point) (second *end*)))
            (progn
             ; (format t "Finished ~a~%" (fourth item))
              (if (< (fourth item) *maxTurns*)
                (defparameter *maxTurns* (fourth item)))
              (setf *endPaths* (append *endPaths* (list item)))
              t)
            (let* ((newPositions (get-new-positions (first item)))
              (filteredNewPositions (remove-if-not (lambda (p) (and (null (gethash (first p) *visisted*)) (null (gethash (first p) *items*)))) newPositions)))
              ;(format t "New positions: ~a~%" filteredNewPositions)
              (let ((found nil))
                (dolist (newPos filteredNewPositions)
                  (let ((point (first newPos)) (direction (second newPos)))
                    (let ((directChangeCount (+ (fourth item) (get-change-increment (second item) direction))))
                      (if (> directChangeCount *maxTurns*)
                        (progn
                        ; (format t "Too long: ~a~%" directChangeCount)
                          nil)
                        (progn
                          (setf (gethash point *visisted*) t)
                          (setf success (move (list point direction item directChangeCount (1+ (fifth item)))))
                          (setf (gethash point *visisted*) nil)
                          (if success
                            (setf found t)
                            (progn
                              (setf (gethash (list point direction) *visitedFailed*) directChangeCount)
                            ))
                        ))))
              )
              found)
            ))))
  ))

(defun calculate-score (path)
  (+ (* (fourth  path) 1000) (1- (count-items-in-chain path)))
)

(defun item-with-minimum-score (items)
  (reduce (lambda (item1 item2)
            (if (< (calculate-score item1) (calculate-score item2))
                item1
                item2))
          items))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *items* (get-items *file-contents*))
(defparameter *visisted* (make-hash-table :test 'equal))
(setf (gethash *start* *visisted*) t)
(defparameter *queue* (list (list *start* (list 0 1) nil 0)))
(defparameter *endPath* nil)
(move-min)

(defparameter *maxTurns* (fourth *endPath*))
(defparameter *visisted* (make-hash-table :test 'equal))
(defparameter *visitedFailed* (make-hash-table :test 'equal))
(defparameter *endPaths* '())
(move (list *start* (list 0 1) nil 0 0))

(defparameter *minScorePath* (item-with-minimum-score *endPaths*))
(defparameter *minScore* (calculate-score *minScorePath*))

(format t "Min Score: ~a~%" *minScore*) ; Part 1

;;;;;;;;;;;;;

(defun flatten-linked-list (item)
  "Flatten a linked list into a single list. Each node's third item points to the next node."
  (if (null item)
      nil
      (append (list (first item)) (flatten-linked-list (third item)))))

(defun append-flattened-lists (list-of-lists)
  "Create an appended list of flattened items from a list of linked lists."
  (reduce #'append (mapcar #'flatten-linked-list list-of-lists)))

(defun points-to-hash-table (points)
  "Convert a list of points to a hash table with points as keys and t as values."
  (let ((hash-table (make-hash-table :test 'equal)))
    (dolist (point points)
      (setf (gethash point hash-table) t))
    hash-table))

(defparameter *bestPaths* (remove-if-not (lambda (path) (= (calculate-score path) *minScore*)) *endPaths*))
(defparameter *appended-flattened-list* (append-flattened-lists *bestPaths*))
(defparameter *unique* (remove-duplicates *appended-flattened-list* :test 'equal))

(format t "Num: ~a~%" (length *unique*)) ; Part 2

;;;;;;;;;;;;;

(render-grid-with-start-end-and-direction *items* *start* *end* (convert-queue-to-hash-table (list *minScorePath*)) *width* *height* ">") ; Part 1
(render-grid-with-start-end-and-direction *items* *start* *end* (points-to-hash-table *unique*) *width* *height* "O")  ; Part 2