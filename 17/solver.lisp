(declaim (optimize (debug 3)))

(defun read-input-array (input)
  (let* ((line-len (position #\Newline input))
         (height (/ (length input) (1+ line-len)))
         (e '(integer 0 9)))
    (make-array (list height line-len)
                :element-type e
                :displaced-to (coerce (loop :for ch :across (remove #\Newline input)
                                            :collect (digit-char-p ch))
                                      `(array ,e (*))))))

(defun manhattan-dist (pos1 pos2)
  (+ (abs (- (car pos1) (car pos2))) (abs (- (cdr pos1) (cdr pos2)))))

(defun opposite (choice) (case choice (:n :s) (:s :n) (:w :e) (:e :w)))

(defun new-pos (pos choice)
  (case choice
    (:n (cons (car pos) (1- (cdr pos))))
    (:s (cons (car pos) (1+ (cdr pos))))
    (:w (cons (1- (car pos)) (cdr pos)))
    (:e (cons (1+ (car pos)) (cdr pos)))))

(defstruct node
  (pos (cons 0 0) :type (cons fixnum fixnum))
  (cost 0 :type fixnum)
  (heur-cost 0 :type fixnum)
  (last-ten (list) :type list))

(defun new-choices (node w h)
  (with-slots (pos last-ten) node
    (let ((last-three-same (and (ignore-errors (not (remove (first last-ten)
                                                            (subseq last-ten 1 3))))
                                (first last-ten))))
      (set-difference (list :n :s :w :e)
                      (list (cond ((= (car pos) 0) :w) ((= (car pos) (1- w)) :e))
                            (cond ((= (cdr pos) 0) :n) ((= (cdr pos) (1- h)) :s))
                            last-three-same
                            (opposite (first last-ten)))))))

(defparameter *new-choices* #'new-choices)

(defun degr-freedom (node)
  (with-slots (last-ten) node
    (let ((first (first last-ten)))
      (loop :for i :from 1 :below 3 :while (eq first (nth i last-ten))
            :finally (return (cons first i))))))

(defparameter *degr-freedom* #'degr-freedom)

(defstruct visited
  (pos (cons 0 0) :type (cons fixnum fixnum))
  (freedom nil :type (cons (or null keyword) fixnum))
  (cost 0 :type fixnum))

(defun equally-or-more-free-p (a b)
  (and (eq (car a) (car b))
       (<= (cdr a) (cdr b))))

(defun same-entries-p (a b)
  (and (equally-or-more-free-p (visited-freedom a) (visited-freedom b))
       (equalp (visited-pos a) (visited-pos b))))

(defun same-nodes-p (a b)
  (and (equalp (node-pos a) (node-pos b))
       (equalp (funcall *degr-freedom* a) (funcall *degr-freedom* b))))

(defun take (n seq)
  (subseq seq 0 (min n (length seq))))

(defun astar (map start end heur)
  (destructuring-bind (h w) (array-dimensions map)
    (let ((open (list (make-node :pos start)))
          (best (make-node :cost most-positive-fixnum))
          (closed (make-hash-table :test #'equalp)))
      (macrolet ((hshkey (node) `(cons (node-pos ,node) (funcall *degr-freedom* ,node))))
        (flet ((neighbors (node)
                 (with-slots (pos cost last-ten) node
                   (let* ((choices (funcall *new-choices* node w h))
                          (new-poses (mapcar (lambda (c) (new-pos pos c)) choices))
                          (step-costs (mapcar (lambda (p) (aref map (cdr p) (car p))) new-poses))
                          (hs (mapcar (lambda (p) (funcall heur p end)) new-poses)))
                     (loop :for better-open-existing := nil
                           :for c :in choices :for npos :in new-poses
                           :for nh :in hs :for sc :in step-costs :for hc :in hs
                           :for new-cost := (+ sc cost)
                           :for new-node := (make-node :cost new-cost :pos npos
                                                       :heur-cost (+ new-cost hc)
                                                       :last-ten (cons c (take 9 last-ten)))
                           :for open-nodes := (remove new-node open :test-not #'same-nodes-p)
                           :for closed-node := (gethash (hshkey new-node) closed)
                           :when open-nodes
                             :do (loop :for open-node :in open-nodes
                                       :if (< new-cost (node-cost open-node))
                                         :do (setf open (remove open-node open :test #'equalp)
                                                   open-node nil)
                                       :else :do (when (<= (node-cost open-node) new-cost)
                                                   (setf better-open-existing t)))
                           :when (and closed-node (< new-cost (visited-cost closed-node)))
                             :do (remhash (hshkey new-node) closed)
                                 (setf closed-node nil)
                           :when (and (not better-open-existing) (not closed-node)
                                      (< (+ new-cost hc) (node-cost best)))
                             :do (push new-node open))
                     (setf open (sort open #'< :key #'node-heur-cost))))))
          (loop :for node := (pop open)
                :do (setf (gethash (hshkey node) closed)
                          (make-visited :pos (node-pos node) :freedom (funcall *degr-freedom* node)
                                        :cost (node-cost node)))
                    (if (equalp (node-pos node) end)
                        (when (< (node-cost node) (node-cost best))
                          (setf best node
                                open (remove-if (lambda (n) (>= (node-heur-cost n) (node-cost best)))
                                                open)))
                        (neighbors node))
                :while open
                :finally (return best)))))))

(defun solve-part-1 (fname)
  (let ((map (read-input-array (uiop:read-file-string fname))))
    (destructuring-bind (w h) (array-dimensions map)
      (astar map (cons 0 0) (cons (1- w) (1- h)) #'manhattan-dist))))

(defun last-four-same (node)
  (with-slots (last-ten) node
    (and (ignore-errors (not (remove (first last-ten) (subseq last-ten 1 4))))
         (first last-ten))))

(defun new-choices-p2 (node w h)
  (with-slots (pos last-ten) node
    (let ((last-four-same (last-four-same node))
          (last-ten-same (and (ignore-errors (not (remove (first last-ten)
                                                          (subseq last-ten 1 10))))
                              (first last-ten))))
      (set-difference (if (not last-four-same)
                          (if (first last-ten) (list (first last-ten)) (list :s :e))
                          (list :n :s :w :e))
                      (list (cond ((= (car pos) 0) :w) ((= (car pos) (1- w)) :e))
                            (cond ((= (cdr pos) 0) :n) ((= (cdr pos) (1- h)) :s))
                            last-ten-same
                            (opposite (first last-ten)))))))

(defun degr-freedom-p2 (node)
  (with-slots (last-ten) node
    (let ((first (first last-ten)))
      (loop :for i :from 1 :below 11 :while (eq first (nth i last-ten))
            :finally (return (cons first i))))))

(defun solve-part-2 (fname)
  (let ((*new-choices* #'new-choices-p2)
        (*degr-freedom* #'degr-freedom-p2))
    (solve-part-1 fname)))
