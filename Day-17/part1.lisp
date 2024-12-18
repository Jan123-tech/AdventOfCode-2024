(defun get-combo-value (val)
  (if (and (>= val 0) (<= val 3)) val
    (if (= val 4) *A*
      (if (= val 5) *B*
        (if (= val 6) *c*
          (format t "Invalid combo: ~A~%" val))))))

(defun adv (operand) ; 0
  (let* ((val (/ *A* (expt 2 (get-combo-value operand)))) (truncated (floor val)))
    (defparameter *A* truncated)))

(defun bxl (operand) ; 1
  (let* ((val (logxor *B* operand)))
    (defparameter *B* val)))

(defun bst (operand) ; 2
  (let* ((val (mod (get-combo-value operand) 8)))
    (defparameter *B* val)))

(defun jnz (operand) ; 3
  (if (= *A* 0)
    nil
    operand))

(defun bxc (operand) ; 4
  (let* ((val (logxor *B* *C*)))
    (defparameter *B* val)))

(defun concatenate-to-output (output new-value)
  "Concatenate a new value to the existing output variable, separated by a comma if the output is not empty."
  (if (string= output "")
      (write-to-string new-value)
      (concatenate 'string output "," (write-to-string new-value))))

(defun out (operand) ; 5
  (let* ((val (mod (get-combo-value operand) 8)))
    (defparameter *output* (concatenate-to-output *output* val))))

(defun bdv (operand) ; 6
  (let* ((val (/ *A* (expt 2 (get-combo-value operand)))) (truncated (floor val)))
    (defparameter *B* truncated)))

(defun cdv (operand) ; 7
  (let* ((val (/ *A* (expt 2 (get-combo-value operand)))) (truncated (floor val)))
    (defparameter *C* truncated)))

(defun factory (op)
  (if (= op 0) #'adv
  (if (= op 1) #'bxl
  (if (= op 2) #'bst
  (if (= op 3) #'jnz
  (if (= op 4) #'bxc
  (if (= op 5) #'out
  (if (= op 6) #'bdv
  (if (= op 7) #'cdv
  (format t "Invalid op: ~A~%" op)
)))))))))

(defun regs (id operand)
  (format t "~a: A: ~a B: ~a C: ~a Index: ~a Operand: ~a~%" id *A* *B* *C* *index* operand))

(defun set-state (regs)
  (progn
    (defparameter *A* (first regs))
    (defparameter *B* (second regs))
    (defparameter *C* (third regs))
    (defparameter *index* 0)
    (defparameter *output* "")))

(defun run (start)
  (let ((regs (first start)) (program (second start)))
    (set-state regs)
    (regs "Start" nil)

    (loop while (< *index* (length program))
        do (let* ((instr (nth *index* program))
          (opCode (first instr)) (operand (second instr))
            (op(factory opCode)))

          (if (eq op #'jnz)
            (let ((result (jnz operand)))
              (if (null result)
                (incf *index*)
                (setf *index* result)))
            (incf *index*))

          (funcall op operand)
          (regs opCode operand)
        )))

    (regs "End" nil)
    (format t "Out: ~A~%" *output*))

;(run '((729 0 0) ((0 1) (5 4) (3 0)))) ; test
(run '((46337277 0 0) ((2 4) (1 1) (7 5) (4 4) (1 4) (0 3) (5 5) (3 0)))) ; puzzle

;(run '((0 2024 43690) ((4 0))))

;If register C contains 9, the program 2,6 would set register B to 1.
;If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
;If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and leave 0 in register A.
;If register B contains 29, the program 1,7 would set register B to 26.
;If register B contains 2024 and register C contains 43690, the program 4,0 would set register B to 44354.