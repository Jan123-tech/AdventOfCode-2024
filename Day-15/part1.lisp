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
                  (defparameter *position* (list i j))
                  (if (not (string-equal c "."))
                    (setf (gethash (list i j) items) c)))))
          (defparameter *instructions* (remove-if-not (lambda (c) (or
            (string-equal c "^")
            (string-equal c "<")
            (string-equal c ">")
            (string-equal c "v"))) (string-to-string-list directions))))))
  items))

(defun update-crate (position direction)
  (let* ((newPosition (new-position position direction))
    (c (gethash (list (first newPosition) (second newPosition)) *items*)))
      (if (string-equal c "#")
        nil
        (if (or (null c) (update-crate newPosition direction))
          (progn
            ;(format t "Moving crate to: ~a~%" newPosition)
            (remhash (list (first position) (second position)) *items*)
            (setf (gethash (list (first newPosition) (second newPosition)) *items*) "O")
            t)
          nil))))

(defun move (start items instructions)
  (let ((position start))
    (dolist (move instructions)
      (let* ((direction (get-direction move)) (newPosition (new-position position direction))
        (c (gethash (list (first newPosition) (second newPosition)) *items*)))
          (if (string-equal c "#")
            (progn
              nil
              ;(format t "Blocked by wall: ~a~%" newPosition)
              )
            (if (string-equal c "O")
              (progn
                (if (update-crate newPosition direction)
                  (progn
                    (setf position newPosition)
                   ; (format t "Push to: ~a~%" position)
                    )
                ;  (format t "Blocked by crate: ~a~%" newPosition)
                  ))
              (progn
                (setf position newPosition)
             
                )))
                  (format t "Move: ~a~%" move)
              ; (render-grid *items* 10 10 position)
                ))))

(defun collect-create-positions (hash-table)
  (let ((keys '()))
    (maphash (lambda (key value)
               (when (string-equal value "O")
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

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *items* (get-items *file-contents*))
(move *position* *items* *instructions*)
(defparameter *crates* (collect-create-positions *items*))
(defparameter *crates-sums* (mapcar (lambda (item) (+ (* (first item) 100) (second item))) *crates*))
(format t "Sum: ~a~%" (reduce #'+ *crates-sums*))