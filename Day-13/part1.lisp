(ql:quickload "split-sequence")

(use-package :split-sequence)

(defstruct b
  x
  y)

(defstruct g
  a
  b
  s)

(defun read-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream) contents)))

(defun split-string-into-integers (input-string)
  (let ((substrings (split-sequence:split-sequence #\Space input-string)))
    (mapcar #'parse-integer substrings)))

(defun split-line-to-pairs (nums)
  (make-g
    :a (make-b :x (first nums) :y (second nums))
    :b (make-b :x (third nums) :y (fourth nums))
    :s (make-b :x (fifth nums) :y (sixth nums))))

(defparameter *file-contents* (read-file "data.txt"))
(defparameter *lines* (split-sequence:split-sequence #\Newline *file-contents*))
(defparameter *numbers* (mapcar #'split-string-into-integers *lines*))
(defparameter *buttons&totals* (mapcar #'split-line-to-pairs *numbers*))

(defun correct (item)
  (= (nth 4 item) 0))

(defun iterate (game)
  (loop with results = '()
        for pressesA from 1 to 100
        do (loop for pressesB from 1 to 100
                 do (push (getMin pressesA pressesB game) results))
        finally (return (remove-if-not (lambda (item) 
                                         (and (correct (first item)) (correct (second item))))
                                       results))))

(defun getMin (pressesA pressesB game)
  (let* (
    (btnA (g-a game)) (btnB (g-b game))
    (btnAx (b-x btnA)) (btnAy (b-y btnA))
    (btnBx (b-x btnB)) (btnBy (b-y btnB))
    (prize (g-s game))
    (prizeX (b-x prize)) (prizeY (b-y prize))
    (btnAxTotal (* pressesA btnAx))
    (btnBxTotal (* pressesB btnBx))
    (btnAyTotal (* pressesA btnAy))
    (btnByTotal (* pressesB btnBy))
    (xTotal (+ btnAxTotal btnBxTotal))
    (yTotal (+ btnAyTotal btnByTotal))
    (xTotalDiff (- prizeX xTotal))
    (yTotalDiff (- prizeY yTotal))
    (costA (* pressesA 3)) (costB pressesB)
    (totalCost (+ costA costB)))
      (list (list pressesA btnAxTotal xTotal prizeX xTotalDiff costA)
        (list pressesB btnBxTotal yTotal prizeY yTotalDiff pressesB costB)
          totalCost)))

(defparameter *mins* (mapcan (lambda (game)
                               (mapcar (lambda (result)
                                         (nth 2 result))
                                       (iterate game)))
                             *buttons&totals*))

(format t "Sum: ~A~%" (reduce #'+ *mins*))