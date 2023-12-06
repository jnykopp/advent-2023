(defun parse-reveal (line)
  (let ((line-parts (remove-if #'uiop:emptyp (uiop:split-string line :separator '(#\Space #\,)))))
    (loop :for (count color) :on line-parts :by #'cddr
          :append (list (intern (string-upcase color) :keyword) (parse-integer count)))))

(defun line-to-game-num-reveals (line)
  (let* ((line-parts (uiop:split-string line :separator '(#\; #\:)))
         (game (first line-parts))
         (game-num (parse-integer (subseq game (length "Game ") (length game)))))
    (list game-num (rest line-parts))))

(defun get-maxs (reveals)
  (let ((maxs (list :red 0 :green 0 :blue 0)))
    (flet ((set-maxs (reveal)
             (loop :for (color) :on maxs :by #'cddr
                   :when (< #1=(getf maxs color) #2=(or (getf reveal color) 0))
                     :do (setf #1# #2#))))
      (loop :for reveal :in (mapcar #'parse-reveal reveals) :do (set-maxs reveal)))
    maxs))

(defun possible-line-p (line max-allowed)
  (destructuring-bind (game-num reveals) (line-to-game-num-reveals line)
    (loop :for (c v) :on (get-maxs reveals) :by #'cddr
          :when (> v (or (getf max-allowed c) 0)) :return nil
            :finally (return game-num))))

(defun solve-part-1 (fname)
  (reduce #'+ (remove-if #'not (mapcar (lambda (x) (possible-line-p x '(:red 12 :green 13 :blue 14)))
                                       (uiop:read-file-lines fname)))))

(defun power-of-line (line)
  (destructuring-bind (game-num reveals) (line-to-game-num-reveals line)
    (declare (ignore game-num))
    (reduce #'* (loop :for (c v) :on (get-maxs reveals) :by #'cddr :collect v))))

(defun solve-part-2 (fname)
  (reduce #'+ (mapcar #'power-of-line (uiop:read-file-lines fname))))
