(declaim (optimize (debug 3)))

(defun next-block-indexes (lines start)
  (let ((next-end (position-if #'uiop:emptyp lines :start start)))
    (cons start (when next-end (1+ next-end)))))

(defun parse-int-line (line)
  (mapcar #'parse-integer (remove-if #'uiop:emptyp (uiop:split-string line))))

(defun parse-init-seeds (line)
  (parse-int-line (subseq line (1+ (position #\: line)))))

(defun name-to-kw (name)
  (intern (string-upcase name) :keyword))

(defun parse-map-name (line)
  (destructuring-bind (from _ to)
      (uiop:split-string (subseq line 0 (search " map:" line)) :separator '(#\-))
    (declare (ignore _))
    (apply #'cons (mapcar #'name-to-kw (list from to)))))

(defun parse-map (lines)
  (loop :with map-name := (parse-map-name (pop lines))
        :for line :in lines
        :unless (uiop:emptyp line) :collect (parse-int-line line) :into values
        :finally (return (cons map-name values))))

(defun parse-all (lines)
  (let ((seeds (parse-init-seeds (first lines)))
        (maps (loop :for (beg . end) := (next-block-indexes lines 2) :then (next-block-indexes lines end)
                    :collect (parse-map (subseq lines beg end))
                    :while end)))
    (cons seeds maps)))

(defun find-value (map value)
  (let ((target-type (cdr (first map)))
        (mappings (rest map)))
    (cons (or (loop :for (dst src len mapping) :in mappings
                    :when (<= src value (1- (+ src len)))
                      :return (+ dst (- value src)))
              value)
          target-type)))

(defun follow-chain (maps type value target)
  (if (eq target type)
      value
      (let* ((corr-map (find type maps :key #'caar))
             (new-val-and-type (find-value corr-map value)))
        (follow-chain maps (cdr new-val-and-type) (car new-val-and-type) target))))

(defun solve-part-1 (fname)
  (destructuring-bind (seeds . maps) (parse-all (uiop:read-file-lines fname))
    (loop :for seed :in seeds :minimizing (follow-chain maps :seed seed :location))))

(defun split-and-translate-int-range (source-beg source-end cut-beg cut-end transl)
  "Return always the intersecting & translated area first, or nil if no intersection."
  (cond ((< source-end cut-beg) (list nil (cons source-beg source-end)))
        ((< cut-end source-beg) (list nil (cons source-beg source-end)))
        ((<= source-end cut-end) (if (< source-beg cut-beg)
                                     (list (cons (+ transl cut-beg) (+ transl source-end))
                                           (cons source-beg (1- cut-beg)))
                                     (list (cons (+ transl source-beg) (+ transl source-end)))))
        ((<= cut-beg source-beg) (list (cons (+ transl source-beg) (+ transl cut-end))
                                       (cons (1+ cut-end) source-end)))
        (t (list (cons (+ transl cut-beg) (+ transl cut-end))
                 (cons source-beg (1- cut-beg))
                 (cons (1+ cut-end) source-end)))))

(defun get-dst-partitions-for-mappings (mappings min-val max-val)
  (let* ((partitions-with-overlaps
           (loop :for (dst src len) :in mappings
                 :collect (split-and-translate-int-range
                           min-val max-val src (+ src (1- len)) (- dst src))))
         (matches (remove-if #'not (mapcar #'first partitions-with-overlaps)))
         (unmatched (loop :for (min . max) :in (first (mapcar #'rest partitions-with-overlaps))
                          :when (and min max)
                            :unless (loop :for (dst src len) :in mappings
                                          :when (first (split-and-translate-int-range
                                                        min max src (+ src (1- len)) 0))
                                            :return t)
                              :collect (cons min max))))
    (append matches unmatched)))

(defun find-value-ranges (map min-val len)
  (let* ((mappings (rest map))
         (dst-value-min-max (get-dst-partitions-for-mappings mappings min-val (+ min-val (1- len)))))
    (mapcar (lambda (minmax) (list #1=(car minmax) (1+ (- (cdr minmax) #1#)))) dst-value-min-max)))

(defun follow-chains (maps type value-ranges target)
  (if (eq target type)
      (loop :for (min len) :in value-ranges :minimizing min)
      (let* ((corr-map (find type maps :key #'caar))
             (mapped-type (cdr (first corr-map)))
             ;; Could optimize by combining possibly overlapping value ranges
             ;; but seems it's unnecessary with this input size.
             (new-val-rngs (loop :for (min-val len) :in value-ranges
                                 :append (find-value-ranges corr-map min-val len))))
        (follow-chains maps mapped-type new-val-rngs target))))

(defun solve-part-2 (fname)
  (destructuring-bind (seeds . maps) (parse-all (uiop:read-file-lines fname))
    (let ((formatted-seeds (loop :for (seed-beg len) :on seeds :by #'cddr :collect (list seed-beg len))))
      (follow-chains maps :seed formatted-seeds :location))))
