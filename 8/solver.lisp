(declaim (optimize (debug 3)))

(defun parse-node (line)
  (destructuring-bind (n l r)
      (remove-if #'uiop:emptyp (uiop:split-string line :separator '(#\Space #\= #\, #\( #\))))
    (cons n (cons l r))))

(defparameter *find-start-nodes* (lambda (nodes) (gethash "AAA" nodes)))

(defun parse-input (fname)
  (let* ((lines (uiop:read-file-lines fname))
         (instrs (pop lines))
         (nodes (make-hash-table :test #'equalp)))
    (loop :for line :in (rest lines)
          :for node := (parse-node line)
          :do (setf (gethash (car node) nodes) node))
    (list (funcall *find-start-nodes* nodes) instrs nodes)))

(defun next-node (instr choices nodes)
  (gethash (case instr (#\L (car choices)) (#\R (cdr choices))) nodes))

(defparameter *final-node-p* (lambda (n) (string= "ZZZ" (car n))))

(defun path-len (first-node instrs nodes)
  (loop :for count :from 0
        :for instr := (aref instrs (mod count (length instrs)))
        :for curr-node := (next-node instr (cdr (or curr-node first-node)) nodes)
        :until (funcall *final-node-p* curr-node)
        :finally (return (1+ count))))

(defun solve-part-1 (fname)
  (apply #'path-len (parse-input fname)))

(defun endswith (str c)
  (char= c (aref str (1- (length str)))))

(defun solve-part-2 (fname)
  (let ((*find-start-nodes*
          (lambda (nodes)
            (loop :for n :being :the :hash-value :of nodes
                  :when (endswith (car n) #\A) :collect n)))
        (*final-node-p* (lambda (n) (endswith (car n) #\Z))))
    (destructuring-bind (start-nodes instrs nodes) (parse-input fname)
      (let ((cycle-lens (loop :for s :in start-nodes :collect (path-len s instrs nodes))))
        ;; Noticed all are divisible by instr len. This makes things
        ;; easier as we don't need to consider cycles starting from
        ;; the middle of instructions.
        (assert (every (lambda (l) (= 0 (mod l (length instrs)))) cycle-lens))
        (apply #'lcm cycle-lens)))))
