(ql:quickload "split-sequence")

(use-package :split-sequence)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun string-to-string-list (str)
  (mapcar #'string (coerce str 'list)))

(defun convert-if-number (char)
  (if (every #'digit-char-p char)
      (parse-integer char)
      char))

(defun split-string-characters (str)
  (mapcar (lambda (char) (convert-if-number(string char))) (coerce str 'list)))

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
            (if (string-equal c "#")
              (setf (gethash (list i j) items) c)))))
  items))

(defun new-position (pos direction)
  (list (+ (first pos) (first direction)) (+ (second pos) (second direction))))

(defun get-new-positions (pos)
  (mapcar (lambda (item) (list (new-position pos item) item)) (get-directions)))

(defun get-directions ()
  (list (list 1 0) (list 0 -1) (list -1 0) (list 0 1) ))

(defun print-hash-table (hash-table)
  (maphash (lambda (key value)
             (if (hash-table-p value)
                 (format t "Key: ~a, Values: ~{~a~^, ~}~%"
                         key
                         (loop for k being the hash-keys of value
                               collect k))
                 (format t "Key: ~a, Value: ~a~%" key value)))
           hash-table))

(defun flatten-linked-list (item)
  "Flatten a linked list into a single list. Each node's third item points to the next node."
  (if (null item)
      nil
      (append (list (second item)) (flatten-linked-list (third item)))))

(defun flatten (lst)
  "Flatten a nested list."
  (cond
    ((null lst) nil)  ;; Base case: empty list
    ((atom (car lst)) (cons (car lst) (flatten (cdr lst))))  ;; If the first element is an atom, add it to the result
    (t (append (flatten (car lst)) (flatten (cdr lst))))))  ;; If the first element is a list, flatten it and append it to the result

(defun move-min (end visited queue walls)
  (if (= (length queue) 0)
    nil
    (let* ((item (pop queue)) (point (first item)))
      ;(format t "Item: ~a~%" item)
      (if (and (= (first point) (first end)) (= (second point) (second end)))
        (progn
          ;(format t "Finished")
          item)
        (let* ((newPositions (get-new-positions (first item)))
          (filteredNewPositions (remove-if (lambda (p)
            (or
              (gethash (first p) visited)
              (gethash (first p) walls))) newPositions)))
         ; (format t "New positions: ~a~%" filteredNewPositions)
          (dolist (newPos filteredNewPositions)
            (let ((point (first newPos)) (direction (second newPos)))
              (setf (gethash point visited) t)
              (setf queue (append queue (list (list point direction item)))))
          )
          (move-min end visited queue walls)))
    )))

(defparameter *file-contents-keypad* (read-file "data-keypad.txt"))
(defparameter *lines-keypad* (split-sequence:split-sequence #\Newline *file-contents-keypad*))
(defparameter *grid-keypad* (mapcar #'split-string-characters *lines-keypad*))
(defparameter *items-keypad* (get-items *file-contents-keypad*))
(defparameter *paths-keypad* (make-hash-table :test 'equal))

(dotimes (i *width*)
  (dotimes (j *height*)
    (dotimes (i0 *width*)
      (dotimes (j0 *height*)
        (if (and (not (and (= i i0) (= j j0))) (null (gethash (list j i) *items-keypad*)) (null (gethash (list j0 i0) *items-keypad*)))
          (progn
            (let* ((route (move-min (list j0 i0) (make-hash-table :test 'equal) (list (list (list j i) nil)) *items-keypad*))
              (directions (reverse (remove nil (flatten-linked-list route)))) (directionsC (mapcar (lambda (item)
                  (if (equal item '(1 0)) "v"
                  (if (equal item '(0 1)) ">"
                  (if (equal item '(-1 0)) "^"
                  "<")))) directions))
                (key (list (lookup-char (list j i) *grid-keypad*) (lookup-char (list j0 i0) *grid-keypad*))))
              (setf (gethash key *paths-keypad*) directionsC))))))))

(defparameter *file-contents-controller* (read-file "data-controller.txt"))
(defparameter *lines-controller* (split-sequence:split-sequence #\Newline *file-contents-controller*))
(defparameter *grid-controller* (mapcar #'split-string-characters *lines-controller*))
(defparameter *items-controller* (get-items *file-contents-controller*))
(defparameter *paths-controller* (make-hash-table :test 'equal))

(dotimes (i *width*)
  (dotimes (j *height*)
    (dotimes (i0 *width*)
      (dotimes (j0 *height*)
        (if (and (not (and (= i i0) (= j j0))) (null (gethash (list j i) *items-controller*)) (null (gethash (list j0 i0) *items-controller*)))
          (progn
            (let* ((route (move-min (list j0 i0) (make-hash-table :test 'equal) (list (list (list j i) nil)) *items-controller*))
              (directions (reverse (remove nil (flatten-linked-list route)))) (directionsC (mapcar (lambda (item)
                  (if (equal item '(1 0)) "v"
                  (if (equal item '(0 1)) ">"
                  (if (equal item '(-1 0)) "^"
                  "<")))) directions))
                (key (list (lookup-char (list j i) *grid-controller*) (lookup-char (list j0 i0) *grid-controller*))))
              (setf (gethash key *paths-controller*) directionsC))))))))

(setf (gethash (list "<" "A") *paths-controller*) (list ">" ">" "^"))
(setf (gethash (list 3 7) *paths-keypad*) (list "<" "<" "^" "^"))

(print-hash-table *paths-keypad*)
(print-hash-table *paths-controller*)

(gethash (list "<" "A") *paths-controller*)

(defun calc (code)

  (defparameter *pointer-0* "A")
  (defparameter *output-0* '())

  (dolist (c (string-to-string-list code))
    (let ((seq (gethash (list (convert-if-number *pointer-0*) (convert-if-number c)) *paths-keypad*)))
      (setf *output-0* (append *output-0* (list (append seq (list "A")))))
      (setf *pointer-0* c)))

  (defparameter *pointer-1* "A")
  (defparameter *output-1* '())

  (dolist (seq0 *output-0*)
    (dolist (c seq0)
      (let ((seq (gethash (list (convert-if-number *pointer-1*) (convert-if-number c)) *paths-controller*)))
        (setf *output-1* (append *output-1* (list (append seq (list "A")))))
        (setf *pointer-1* c))))

  (defparameter *pointer-2* "A")
  (defparameter *output-2* '())

  (dolist (seq0 *output-1*)
    (dolist (c seq0)
      (format t "key: ~a move: ~a~%" (list (convert-if-number *pointer-2*) (convert-if-number c)) (gethash (list (convert-if-number *pointer-2*) (convert-if-number c)) *paths-controller*))
      (let ((seq (gethash (list (convert-if-number *pointer-2*) (convert-if-number c)) *paths-controller*)))
        (setf *output-2* (append *output-2* (list (append seq (list "A")))))
        (setf *pointer-2* c))))

  (format t "Output: ~a~%" *output-0*)
  (format t "Output: ~a~%" *output-1*)
  ;(format t "Output: ~a~%" *output-2*)
      
  *output-2*)


(format t "Output Final: ~a~%" (calc "379A"))
(length (flatten (calc "029A")))
(length (flatten (calc "980A")))
(length (flatten (calc "179A")))
(length (flatten (calc "456A")))
(length (flatten (calc "379A")))

(print-hash-table *paths-keypad*)

(v < < A) (> > ^ A) (v A) (^ A) (v < A) (< A) (A) (> > ^ A) (A)
               (v A) (^ < A) (> A) (A) (v A) (^ A) (v < A) (^ > A) (A) (< A)
               (> A) (v < A) (< A) (> > ^ A) (A) (A) (< A) (> v A) (^ A))

<v<A (v < < A)

>>^A (> > ^ A)

vA (v A)

^A (^ A)

<vA<A (v < A)(< A)

A (A)

>>^A (> > ^ A)

A (A)

vA (v A)

<^A (^ < A)

>A (> A)

AvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A

(length (flatten (calc "286A")))
(length (flatten (calc "480A")))
(length (flatten (calc "140A")))
(length (flatten (calc "413A")))
(length (flatten (calc "964A")))


((^ A)                              ( < < ^ ^ A)                             (> > A)           (v v v A))
(< A)(> A)                          (v<AA              (<AA  (> > ^ A)
(v < < A) (> > ^ A) (v A) (^ A)     v<A<A) (> > ^ AA (v < < A) (> > ^ AA 
         
 
