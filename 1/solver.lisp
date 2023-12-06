(defun find-num (line)
  (loop :with first := nil :with last := nil
        :for char :across line :for int := (digit-char-p char)
        :when int :do (if first (setf last int) (setf first int))
        :finally (return (+ (* 10 first) (or last first)))))

(defun solve-part-1 (fname)
  (reduce #'+ (mapcar #'find-num (uiop:read-file-lines fname))))

(defparameter *text-nums* (loop :for i :from 1 :upto 9 :collect (cons i (format nil "~R" i))))

(defun find-textual-num (line offset num-set)
  (loop :for (val . txt) :in num-set :for max-offs := (min (+ offset (length txt)) (length line))
        :if (string= (subseq line offset max-offs) txt)
          :return val))

(defun find-num-part2 (line)
  (declare (optimize (debug 3)))
  (loop :with first := nil :with last := nil :for index :from 0 :below (length line)
        :for int := (or (digit-char-p (aref line index)) (find-textual-num line index *text-nums*))
        :when int :do (if first (setf last int) (setf first int))
        :finally (return (+ (* 10 first) (or last first)))))

(defun solve-part-2 (fname)
  (reduce #'+ (mapcar #'find-num-part2 (uiop:read-file-lines fname))))
