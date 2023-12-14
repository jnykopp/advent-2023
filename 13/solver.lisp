(defun read-input-array (input)
  (let* ((line-len (1+ (position #\Newline input)))
         (height (/ (length input) line-len)))
    (make-array (list height line-len) :element-type 'character :displaced-to input)))

(defun parse-maps (input)
  (let ((limit (search (uiop:strcat #\Newline #\Newline) input)))
    (if limit
        (cons (read-input-array (subseq input 0 (1+ limit)))
              (parse-maps (subseq input (+ limit 2))))
        (list (read-input-array input)))))

(defun is-refl-p (line point)
  (loop :for l :from point :downto 0 :for r :from (1+ point) :below (length line)
        :unless (char= (aref line l) (aref line r)) :do (return nil)
        :finally (return point)))

(defun get-line (arr dim i)
  (coerce (loop :for n :below (- (array-dimension arr dim) dim)
                :collect (apply #'aref arr (if (= dim 0) (list n i) (list i n))))
          'string))

(defun poss (n) (loop :for x :from 0 :below n :collect x))

(defun find-refls-by-dim (map dim &optional ignore)
  (let* ((other-dim (if (= 0 dim) 1 0))
         (ignore-by-dim (ignore-errors (1- (nth dim ignore))))
         ;; removing dim and other-dim takes care of newlines in end of each row
         (refl-points (poss (- (array-dimension map other-dim) other-dim 1)))
         (num-lines (- (array-dimension map dim) dim)))
    (loop :for row :from 0 :below num-lines
          :while (> (length refl-points) 0)
          :do (setf refl-points (remove nil (mapcar (lambda (n)
                                                      (is-refl-p (get-line map other-dim row) n))
                                                    refl-points))))
    (ignore-errors (1+ (first (remove ignore-by-dim refl-points))))))

(defun find-refls (map &optional ignore)
  (mapcar (lambda (d) (find-refls-by-dim map d ignore)) '(0 1)))

(defparameter *find-func* #'find-refls)

(defun solve-part-1 (fname)
  (let* ((patterns (mapcar *find-func* (parse-maps (uiop:read-file-string fname))))
         (scores (mapcar (lambda (x) (+ (or (first x) 0) (* 100 (or (second x) 0)))) patterns)))
    (reduce #'+ scores)))

(defun find-smudgy-refls (smudgy-map)
  (let ((old-refls (find-refls smudgy-map)))
    (loop :for y :below (array-dimension smudgy-map 0)
          :when (loop :for x :below (1- (array-dimension smudgy-map 1))
                      :for ch := #1=(aref smudgy-map y x)
                      :do (setf #1# (if (char= ch #\#) #\. #\#))
                          (let ((refls (find-refls smudgy-map old-refls)))
                            (unless (or (every #'null refls)
                                        (equalp refls old-refls))
                              (return refls))
                            (setf #1# ch)))
            :return it)))

(defun solve-part-2 (fname)
  (let ((*find-func* #'find-smudgy-refls))
    (solve-part-1 fname)))
