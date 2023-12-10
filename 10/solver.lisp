(defun read-input-array (fname)
  (let* ((array (uiop:read-file-string fname))
         (line-len (1+ (position #\Newline array)))
         (height (/ (length array) line-len))
         (s-pos (multiple-value-bind (y x) (floor (position #\S array) line-len) (cons y x))))
    (list (make-array (list height line-len) :element-type 'character :displaced-to array) s-pos)))

(defun upd-pos (dir pos)
  (let ((p (copy-tree pos)))
    (case dir (#\u (decf (car p))) (#\d (incf (car p))) (#\l (decf (cdr p))) (#\r (incf (cdr p))))
    p))

(defmacro tile-at (array pos) ; macro for setf-ability
  `(aref ,array (car ,pos) (cdr ,pos)))

(defparameter *rules* '(((#\u #\|) . #\u) ((#\u #\7) . #\l) ((#\u #\F) . #\r)
                        ((#\d #\|) . #\d) ((#\d #\L) . #\r) ((#\d #\J) . #\l)
                        ((#\l #\-) . #\l) ((#\l #\L) . #\u) ((#\l #\F) . #\d)
                        ((#\r #\-) . #\r) ((#\r #\7) . #\d) ((#\r #\J) . #\u)))

(defun move (array pos dir)
  (let* ((next-pos (upd-pos dir pos))
         (next-tile (tile-at array next-pos)))
    (cons next-pos (cdr (assoc (list dir next-tile) *rules* :test #'equalp)))))

(defun possible-start-dirs (array s-pos)
  (loop :for dir :in '(#\u #\d #\l #\r)
        :when (cdr (move array s-pos dir)) :collect dir))

(defun find-loop (array s-pos)
  ;; Noticed input has only two possible directions from S (no
  ;; dead-ends as red herrings). It's the same loop but different
  ;; direction. Hence, just pick one.
  (loop :for loop-len :from 0
        :for (pos . dir) := (cons s-pos (first (possible-start-dirs array s-pos)))
          :then (move array pos dir)
        :until (and (> loop-len 0) (equalp pos s-pos))
        :finally (return loop-len)))

(defun solve-part-1 (fname)
  (/ (apply #'find-loop (read-input-array fname)) 2))

(defun dir-and-turn-to-up-down (dir turn)
  (if (find dir '(#\u #\d))
      dir
      (let ((turn-rules (remove-if-not (lambda (x) (char= (cadar x) turn)) *rules*)))
        (caar (find dir turn-rules :key #'cdr)))))

(defun mark-loop-for-sweepline (array s-pos)
  (let ((s-dirs (possible-start-dirs array s-pos))
        (turns '(#\F #\J #\7 #\L)))
    (loop :for loop-length :from 0
          :for (pos . dir) := (cons s-pos (first s-dirs)) :then (move array pos dir)
          :for tile := (tile-at array pos)
          :do (setf (tile-at array pos) #\x)
          :if (and (find dir '(#\u #\d)) (char= tile #\|))
            :do (setf (tile-at array pos) dir)
          :else 
            :when (find tile turns)
              :do (setf (tile-at array pos) (dir-and-turn-to-up-down dir tile))
          :until (and (> loop-length 0) (equalp pos s-pos)))
    array))

(defun sweep (array)
  (loop :for l :from 0 :below (array-dimension array 0)
        :sum (loop :with inside :with curr-dir
                   :for c :from 0 :below (array-dimension array 1)
                   :for tile := (tile-at array (cons l c))
                   :when (find tile '(#\u #\d))
                     :when (or (not curr-dir) (not (char= curr-dir tile)))
                       :do (setf inside (not inside) curr-dir tile)
                   :when (and inside (not (find tile '(#\u #\d #\x))))
                     :do (setf (tile-at array (cons l c)) #\I) :and :sum 1)))

(defun solve-part-2 (fname)
  (sweep (apply #'mark-loop-for-sweepline (read-input-array fname))))
