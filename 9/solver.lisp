(declaim (optimize (debug 3)))

(defun parse-int-line (line &optional (reverse-p t))
  (funcall (if reverse-p #'reverse #'identity)
           (mapcar #'parse-integer (remove-if #'uiop:emptyp (uiop:split-string line)))))

(defun gen-next-diff (hist)
  (loop :for i1 := (first hist) :then i2 :for i2 :in (rest hist)
        :collect (- i1 i2)))

(defun gen-diffs (hist)
  (reverse (append (list hist)
                   (loop :for next := (gen-next-diff (or next hist))
                         :collect next
                         :until (every (lambda (n) (= 0 n)) next)))))

(defun generate-hist (seqs)
  (loop :for seq :in seqs :for new-elt := 0 :then (+ (first seq) new-elt)
        :finally (return new-elt)))

(defun solve-part-1 (fname)
  (let ((hists (mapcar #'parse-int-line (uiop:read-file-lines fname))))
    (reduce #'+ (mapcar #'generate-hist (mapcar #'gen-diffs hists)))))

(defun solve-part-2 (fname)
  (let ((hists (mapcar (lambda (l) (parse-int-line l nil)) (uiop:read-file-lines fname))))
    (reduce #'+ (mapcar #'generate-hist (mapcar #'gen-diffs hists)))))
