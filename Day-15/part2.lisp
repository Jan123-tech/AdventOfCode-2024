(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun split-on-double-line-break (input)
  (let* ((split-pos (search (concatenate 'string (string #\Newline) (string #\Newline)) input))
         (map-part (if split-pos (subseq input 0 split-pos) ""))
         (directions-part (if split-pos (subseq input (+ split-pos 2)) "")))
    (values map-part directions-part)))

(defun string-to-string-list (str)
  (mapcar #'string (coerce str 'list)))

(defun lookup-char (pos grid)
  (nth (cdr pos) (nth (first pos) grid)))

(defun get-direction (c)
  (if (string-equal c "^")
    (list -1 0)
    (if (string-equal c ">")
      (list 0 1)
      (if (string-equal c "v")
        (list 1 0)
        (if (string-equal c "<")
          (list 0 -1)
          (print "Illegal Character")
        )))))

(defun new-position (pos direction)
  (list (+ (first pos) (first direction)) (+ (second pos) (second direction))))

(defun print-hash-table (hash-table)
  (maphash (lambda (key value)
             (if (hash-table-p value)
                 (format t "Key: ~a, Values: ~{~a~^, ~}~%"
                         key
                         (loop for k being the hash-keys of value
                               collect k))
                 (format t "Key: ~a, Value: ~a~%" key value)))
           hash-table))

(defun get-items (str)
  (let ((items (make-hash-table :test 'equal)))
    (multiple-value-bind (map directions) (split-on-double-line-break str)
      (let* ((lines (split-sequence:split-sequence #\Newline map))
        (grid (mapcar #'string-to-string-list lines)))
          (dotimes (i (length grid))
            (dotimes (j (length (first grid)))
              (let ((c (lookup-char (cons i j) grid)))
                (if (string-equal c "@")
                  (progn
                    (defparameter *position* (list i (* j 2))))
                  (if (not (string-equal c "."))
                    (if (string-equal c "O")
                      (progn
                        (setf (gethash (list i (* j 2)) items) "[")
                        (setf (gethash (list i (1+ (* j 2))) items) "]"))
                      (progn
                        (setf (gethash (list i (* j 2)) items) c)
                        (setf (gethash (list i (1+ (* j 2))) items) c)))
                    nil))))))
      (defparameter *instructions* (remove-if-not (lambda (c) (or
        (string-equal c "^")
        (string-equal c "<")
        (string-equal c ">")
        (string-equal c "v"))) (string-to-string-list directions))))
  items))

(defun collect-create-positions (hash-table)
  (let ((keys '()))
    (maphash (lambda (key value)
               (when (string-equal value "[")
                 (push key keys)))
             hash-table)
    keys))

(defun render-grid (coordinates width height point)
  "Render a grid with the given coordinates, width, height, and an additional point rendered as '@'."
  (let ((grid (make-array (list height width) :initial-element #\.)))
    ;; Populate the grid with the specified characters
    (maphash (lambda (key value)
               (setf (aref grid (first key) (second key)) value))
             coordinates)
    ;; Set the additional point
    (setf (aref grid (first point) (second point)) #\@)
    ;; Print the grid
    (dotimes (i height)
      (dotimes (j width)
        (princ (aref grid i j)))
      (terpri))))

(defun move (start items instructions)
  (let ((position start) (index 0))
    (dolist (move instructions)
      (let* ((direction (get-direction move)) (newPosition (new-position position direction))
        (c (gethash (list (first newPosition) (second newPosition)) *items*)))
         ; (format t "Direction: ~a~%" direction)          
          (if (string-equal c "#")
            (progn
              nil
            ;  (format t "Blocked by wall: ~a~%" newPosition)
              )
            (if (or (string-equal c "[") (string-equal c "]"))
              (progn
                (if (or (and (= (first direction) 0) (update-crate-horizontal newPosition direction c))
                  (and (= (second direction) 0) (update-crate-vertically newPosition direction c nil)))
                  (progn
                    (if (and (= (second direction) 0))
                      (update-crate-vertically newPosition direction c t))
                    (setf position newPosition)
                 ;   (format t "Push to: ~a~%" position)
                    )
                ;  (format t "Blocked by crate: ~a~%" newPosition)
                  ))
              (progn
                (setf position newPosition)
             
                )))
                (setf index (1+ index))
               ;   (format t "No.~a~% Move: ~a~% " index move)
              ;(render-grid *items* 20 10 position)
              
                ))))

(defun update-crate-horizontal (position direction oldC)
  (let* ((newPosition (new-position position direction))
    (c (gethash (list (first newPosition) (second newPosition)) *items*)))
      (if (string-equal c "#")
        nil
        (if (or (null c) (update-crate-horizontal newPosition direction c))
          (progn
          ;  (format t "Moving crate to: ~a~%" newPosition)
            (remhash (list (first position) (second position)) *items*)
            (setf (gethash (list (first newPosition) (second newPosition)) *items*) oldC)
            t)
          nil))))

(defun update-crate-vertically (position direction oldC withUpdate)
  (let* ((otherPosition (list (first position) (+ (second position) (if (string-equal oldC "]") -1 1))))
    (newPosition (new-position position direction)) (otherNewPosition (new-position otherPosition direction))
    (c (gethash (list (first newPosition) (second newPosition)) *items*))
    (c2 (gethash (list (first otherNewPosition) (second otherNewPosition)) *items*)))
      (if (or (string-equal c "#") (string-equal c2 "#"))
        nil
        (if (or
          (and (null c) (null c2))
            (and (null c) (update-crate-vertically otherNewPosition direction c2 withUpdate))
              (and (null c2) (update-crate-vertically newPosition direction c withUpdate))
                (and (string-equal oldC c) (update-crate-vertically newPosition direction c withUpdate))
                    (and (update-crate-vertically newPosition direction c withUpdate) (update-crate-vertically otherNewPosition direction c2 withUpdate)))
          (progn
           ; (format t "Moving crate to: ~a~%" newPosition)
           ; (format t "Moving other crate to: ~a~%" otherNewPosition)
            (if withUpdate
              (progn
                (remhash (list (first position) (second position)) *items*)
                (remhash (list (first otherPosition) (second otherPosition)) *items*)
                (setf (gethash (list (first newPosition) (second newPosition)) *items*) oldC)
                (setf (gethash (list (first otherNewPosition) (second otherNewPosition)) *items*) (if (string-equal oldC "]") "[" "]"))))
            t)
          nil))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *items* (get-items *file-contents*))
;(render-grid *items* 20 10 *position*)
(move *position* *items* *instructions*)

(defparameter *crates* (collect-create-positions *items*))
(defparameter *crates-sums* (mapcar (lambda (item) (+ (* (first item) 100) (second item))) *crates*))
(format t "Sum: ~a~%" (reduce #'+ *crates-sums*))

;(print-hash-table *items*)