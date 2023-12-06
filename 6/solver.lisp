(declaim (optimize (debug 3)))

(defun parse-int-line (line)
  (mapcar #'parse-integer (remove-if #'uiop:emptyp (uiop:split-string line))))

(defun parse-line (line parse-fun)
  (funcall parse-fun (subseq line (1+ (position #\: line)))))

(defun parse-input (lines parse-fun)
  (let ((times (parse-line (pop lines) parse-fun))
        (dists (parse-line (pop lines) parse-fun)))
    (loop :for time :in times :for dist :in dists
          :collect (cons time dist))))

(defun distance (charge-time travel-time)
  (* charge-time travel-time))

(defun ways-to-beat (time-dist)
  (destructuring-bind (time . record-dist) time-dist
    (loop :for charge-time :from 1 :below time
          :when (> (distance charge-time (- time charge-time)) record-dist)
            :summing 1)))

(defun solve-part-1 (fname)
  (reduce #'* (mapcar #'ways-to-beat (parse-input (uiop:read-file-lines fname) #'parse-int-line))))

(defun parse-int-ignore-spaces (line)
  (list (parse-integer (remove #\Space line))))

(defun solve-part-2 (fname)
  (ways-to-beat (first (parse-input (uiop:read-file-lines fname) #'parse-int-ignore-spaces))))
