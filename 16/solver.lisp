(defun read-input-array (input)
  (let* ((line-len (position #\Newline input))
         (height (/ (length input) (1+ line-len))))
    (make-array (list height line-len) :element-type 'character
                                       :displaced-to (remove #\Newline input))))

(defstruct beam
  (pos (cons 0 0) :type cons)
  (dir :e :type keyword))

(defun new-pos (pos dir)
  (declare (optimize (speed 3))
           (type (cons fixnum fixnum) pos))
  (case dir
    (:n (cons (car pos) (1- (cdr pos))))
    (:s (cons (car pos) (1+ (cdr pos))))
    (:w (cons (1- (car pos)) (cdr pos)))
    (:e (cons (1+ (car pos)) (cdr pos)))
    (t pos)))

(defun pos-in-map-p (pos map)
  (declare (optimize (speed 3))
           (type (cons fixnum fixnum) pos))
  (destructuring-bind (width height) (array-dimensions map)
    (not (or (< (car pos) 0) (< (cdr pos) 0)
             (>= (car pos) width) (>= (cdr pos) height)))))

(defparameter *visited* nil)
(defparameter *beams* nil)

(defun next-step (beam map)
  (declare (optimize (speed 3))
           (type (array character) map))
  (with-slots (pos dir) beam
    (let ((thing (aref map (cdr pos) (car pos))))
      (flet ((new-beam (new-dir)
               (push (make-beam :pos pos :dir new-dir) *beams*)))
        (if (find (cons pos dir) *visited* :test #'equalp)
            (setf dir :end)
            (progn
              (push (cons pos dir) *visited*)
              (case thing
                (#\/ (setf dir (case dir (:n :e) (:e :n) (:s :w) (:w :s))))
                (#\\ (setf dir (case dir (:n :w) (:w :n) (:s :e) (:e :s))))
                (#\| (when (find dir '(:w :e)) (setf dir :s) (new-beam :n)))
                (#\- (when (find dir '(:n :s)) (setf dir :e) (new-beam :w))))))))))

(defun follow-one-beam (beam map)
  (declare (optimize (speed 3)))
  (with-slots (pos dir) beam
    (loop :until (or (eq dir :end) (not (pos-in-map pos map)))
          :do (next-step beam map)
              (setf pos (new-pos pos dir)))))

(defun follow-beams (beam map)
  (let ((*beams* (list beam))
        *visited*)
    (loop :for beam := (pop *beams*)
          :do (follow-one-beam beam map)
          :while *beams*)
    (length (delete-duplicates (mapcar #'car *visited*) :test #'equalp))))

(defun solve-part-1 (fname)
  (let ((map (read-input-array (uiop:read-file-string fname))))
    (follow-beams (make-beam) map)))

(defun solve-part-2 (fname)
  ;; This is unelegant brute force. Not too slow though, few minutes to run. Could be made faster
  ;; with caching paths & caching new beam spawns for each beam, but running out of time for that.
  (let ((map (read-input-array (uiop:read-file-string fname))))
    (destructuring-bind (h w) (array-dimensions map)
      (loop :for (start-pos . dir)
              :in (append (loop :for x :below w :collect (cons (cons x 0) :s))
                          (loop :for x :below w
                                :collect (cons (cons x (1- h)) :n))
                          (loop :for y :below h :collect (cons (cons 0 y) :e))
                          (loop :for y :below h :collect (cons (cons (1- w) y) :w)))
            :maximize (follow-beams (make-beam :pos start-pos :dir dir) map)))))
