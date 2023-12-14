(defparameter *mult* 1)

(defun parse-line (line)
  (destructuring-bind (springs arr-str) (uiop:split-string line)
    (let ((int-list (mapcar #'parse-integer (uiop:split-string arr-str :separator '(#\,)))))
      (loop :with res :repeat *mult* :for i :from 1
            :do (setf res (uiop:strcat res springs (unless (= i *mult*) "?")))
            :append int-list :into ints
            :finally (return (cons res ints))))))

(defun groups-min-len (groups-left)
  (+ (reduce #'+ groups-left) (length groups-left) -1))

(defun group-possible-at (input index group groups-remaining-p)
  (and (loop :for i :from index :below (+ index group) :for c := (aref input i)
             :if (not (or (char= c #\?) (char= c #\#)))
               :return nil
             :finally (return t))
       (or (not groups-remaining-p)
           (< (length input) (+ index group))
           (let ((c (aref input (+ index group))))
             (or (char= #\. c) (char= #\? c))))))

(defun make-group-str (group groups-remaining-p)
  (uiop:strcat (make-string group :initial-element #\#) (when groups-remaining-p #\.)))

(defparameter *cache* (make-hash-table :test #'equal))

(defun generate-guess (input-left orig-groups-left)
  (declare (optimize (debug 3)))
  (let ((groups-left (copy-tree orig-groups-left)))
    (or (gethash (cons input-left orig-groups-left) *cache*)
        (loop :with i := 0
              :while (and (<= i (length input-left))
                          (>= (- (length input-left) i) (groups-min-len groups-left)))
              :unless groups-left
                :return (loop :for c :across (subseq input-left i)
                          :unless (or (char= c #\.) (char= c #\?)) :return 0
                            :finally (setf (gethash (cons input-left orig-groups-left) *cache*) 1)
                                     (return 1))
              :do (case (aref input-left i)
                    (#\. (incf i))
                    (#\# (if (group-possible-at input-left i (first groups-left) (rest groups-left))
                             (progn (incf i (+ (first groups-left) (if (rest groups-left) 1 0)))
                                    (pop groups-left))
                             (return 0)))
                    (#\? (return
                           (setf (gethash (cons input-left orig-groups-left) *cache*)
                                 (+ (generate-guess (subseq input-left (1+ i)) groups-left)
                                    (generate-guess (uiop:strcat "#" (subseq input-left (1+ i))) groups-left))))))
              :finally (return 0)))))

(defun solve-part-1 (fname)
  (reduce #'+ (mapcar (lambda (x) (generate-guess (car x) (cdr x))) (mapcar #'parse-line (uiop:read-file-lines fname)))))

(defun solve-part-2 (fname)
  (let ((*mult* 5))
    (solve-part-1 fname)))
