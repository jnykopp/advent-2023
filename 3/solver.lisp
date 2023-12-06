(declaim (optimize (debug 3)))

(defun read-input-array (fname)
  (let* ((array (uiop:read-file-string fname))
         (line-len (1+ (position #\Newline array)))
         (height (/ (length array) line-len)))
    ;; Will always have newline as last item in each line
    (make-array (list height line-len) :element-type 'character :displaced-to array)))

(defun sym-p (char)
  (and (not (digit-char-p char)) (not (find char '(#\. #\Newline) :test #'char=))))

(defun surrounded-by-p (array num-y num-x num-len predicate)
  (flet ((sym-on-row (y) (loop :for x :from (max 0 (1- num-x)) :upto (+ num-x num-len)
                               :when (funcall predicate (aref array y x))
                                 :return (+ x (* y (array-dimension array 1))))))
    (or (when (>= #1=(1- num-y) 0) (sym-on-row #1#))
        (sym-on-row num-y)
        (when (< #2=(1+ num-y) (array-dimension array 0)) (sym-on-row #2#)))))

(defun array-slice (array y x len)
  (make-array len :element-type 'character :displaced-to array
                  :displaced-index-offset (+ (* y (array-dimension array 1)) x)))

(defun find-numbers (array)
  (destructuring-bind (max-y max-x) (array-dimensions array)
    (loop :for y :from 0 :below max-y
          :append (loop :for x :from 0 :below max-x
                        :for num-str := (loop :with beg-x := x
                                              :for digit := (digit-char-p (aref array y x))
                                              :while (and digit (<= x max-x)) :do (incf x)
                                              :finally (return (when (/= beg-x x)
                                                                 (array-slice array y beg-x (- x beg-x)))))
                        :when (and num-str (surrounded-by-p array y (- x #1=(length num-str)) #1# #'sym-p))
                          :collect (parse-integer num-str)))))

(defun solve-part-1 (fname)
  (reduce #'+ (find-numbers (read-input-array fname))))

(defun gear-p (char)
  (char= char #\*))

(defun calc-gear-numbers (array)
  (destructuring-bind (max-y max-x) (array-dimensions array)
    (let ((gear-pos-num-map (make-hash-table)))
      (loop :for y :from 0 :below max-y
            :append (loop :for x :from 0 :below max-x
                          :for num-str := (loop :with beg-x := x
                                                :for digit := (digit-char-p (aref array y x))
                                                :while (and digit (<= x max-x)) :do (incf x)
                                                :finally (return (when (/= beg-x x)
                                                                   (array-slice array y beg-x (- x beg-x)))))
                          ;; assume just one gear per number (`surrounded-by-p' returns the first)
                          :for gear-pos := (and num-str (surrounded-by-p array y (- x #1=(length num-str)) #1# #'gear-p))
                          :when gear-pos
                            :do (push (parse-integer num-str) (gethash gear-pos gear-pos-num-map))))
      (loop :for v :being :the :hash-values :in gear-pos-num-map
            :when (> (length v) 1) :summing (reduce #'* v)))))

(defun solve-part-2 (fname)
  (calc-gear-numbers (read-input-array fname)))
