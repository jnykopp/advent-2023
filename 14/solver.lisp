(declaim (optimize (debug 3)))

(defstruct line
     (axis :nan :type keyword)
     (num 0 :type fixnum)
     (stops nil :type list))

(defun parse-input (fname)
  (let (rounds squares height width)
    (loop :for y :from 0
          :for line :in (uiop:read-file-lines fname)
          :do (loop :for x :from 0 :for ch :across line
                    :do (case ch
                          (#\# (push (cons x y) squares))
                          (#\O (push (cons x y) rounds)))
                    :finally (setf width x))
          :finally (setf height y))
    (list rounds squares width height)))

(defun on-line (objs n dim)
  (let ((parallel (if (eq dim :x) #'car #'cdr))
        (perpendicular (if (eq dim :x) #'cdr #'car)))
    (sort (mapcar parallel (remove-if (lambda (m) (/= n m)) objs :key perpendicular)) #'<)))

(defun generate-rows-columns (width height squares)
  (loop :for dim :in (list height width) :for axis :in '(:x :y) :for max :in (list width height)
        :for parallel :in (list #'cdr #'car) :for perpendicular :in (list #'car #'cdr)
        :collect (loop :for n :below dim
                       :collect (make-line
                                 :axis axis :num n
                                 :stops (append
                                          (cons -1 (on-line squares n axis))
                                          (list max))))))

(defmacro if-descending (then else) `(if (find dir '(:e :s)) ,then ,else))

(defun move-on-line (rounds-on-line line-stops dir)
  (labels ((maybe-reverse (l) (if-descending (reverse l) l))
           (between (s1 s2 o) (< (min s1 s2) o (max s1 s2))))
    (let ((delta (if-descending -1 1))
          (stops (maybe-reverse line-stops))
          (rounds (maybe-reverse rounds-on-line)))
      (loop :for (prev-stop next-stop) :on stops
            :while next-stop
            :append
            (let ((rnds (remove-if-not (lambda (o) (between prev-stop next-stop o)) rounds)))
              (loop :repeat (length rnds) :for p := (+ prev-stop delta) :then (+ p delta)
                    :collect p))))))

(defun move (rounds x-lines y-lines dir)
  (let* ((axis (case dir ((:n :s) :y) ((:w :e) :x))))
    (loop :for line :in (if (eq axis :x) x-lines y-lines)
          :for rounds-on-line := (on-line rounds (line-num line) axis)
          :append (let ((new-coords (move-on-line rounds-on-line (line-stops line) dir)))
                    (mapcar (lambda (n) (if (eq axis :x)
                                            (cons n (line-num line))
                                            (cons (line-num line) n)))
                            new-coords)))))

(defun total-load (rounds height)
  (loop :for y :from 0 :below height :for l :from height :downto 1
        :sum (* l (length (on-line rounds y :x)))))

(defun solve-part-1 (fname)
  (destructuring-bind (rounds squares width height) (parse-input fname)
    (destructuring-bind (x-lines y-lines) (generate-rows-columns width height squares)
      (total-load (move rounds x-lines y-lines :n) height))))

(defun solve-part-2 (fname)
  (destructuring-bind (rounds squares width height) (parse-input fname)
    (destructuring-bind (x-lines y-lines) (generate-rows-columns width height squares)
      (let* ((target-cycles 1000000000)
             (seen-states (make-hash-table :test #'equalp))
             (tilt-seq '(:n :w :s :e)))
        (destructuring-bind (cycle-start . cycle-end)
            (loop :for i :below target-cycles
                  :do (setf #1=(gethash rounds seen-states) i)
                      (loop :for dir :in tilt-seq
                            :do (setf rounds (move rounds x-lines y-lines dir)))
                  :when #1# :return (cons #1# (1+ i)))
          (let* ((cycle-length (- cycle-end cycle-start))
                 (state-at-target (+ (mod (- target-cycles cycle-start) cycle-length) cycle-start))
                 (rounds-at-state (loop :for key :being :the :hash-keys :of seen-states
                                          :using (hash-value value)
                                        :when (= state-at-target value) :return key)))
            (total-load rounds-at-state height)))))))
