(defun hash (string)
  (loop :with v := 0 :for ch :across string
        :do (setf v (mod (* (+ v (char-code ch)) 17) 256)) :finally (return v)))

(defun solve-part-1 (fname)
  (reduce #'+ (mapcar #'hash (uiop:split-string (remove #\Newline (uiop:read-file-string fname))
                                                :separator '(#\,)))))

(defstruct oper
  (label "")
  (cmd :none)
  (lens 0))

(defun parse-label-cmd (str)
  (if (uiop:string-suffix-p str #\-)
      (make-oper :label (subseq str 0 (- #1=(length str) 1)) :cmd :del)
      (make-oper :label (subseq str 0 (- #1# 2)) :cmd :add
                 :lens (parse-integer (string (aref str (- #1# 1)))))))

(defun parse-input (string)
  (let ((label-cmds (uiop:split-string (remove #\Newline string) :separator '(#\,))))
    (mapcar #'parse-label-cmd label-cmds)))

(defstruct lens
  (label "")
  (lens 0))

(defun mod-list (content oper)
  (if (eq (oper-cmd oper) :add)
      (let ((old (find (oper-label oper) content :key #'lens-label :test #'string=)))
        (if old
            (progn (setf (lens-lens old) (oper-lens oper)) content)
            (concatenate 'list content (list (make-lens :label (oper-label oper)
                                                        :lens (oper-lens oper))))))
      (remove (oper-label oper) content :key #'lens-label :test #'string=)))

(defun modify-boxes (boxes oper)
  (setf #1=(aref boxes (hash (oper-label oper))) (mod-list #1# oper)))

(defun calc-focusing-power (boxes)
  (loop :for bn :below 256
        :for content := (aref boxes bn)
        :sum (loop :for i :from 1 :for lens :in content
                   :sum (* (1+ bn) i (lens-lens lens)))))

(defun solve-part-2 (fname)
  (let ((boxes (make-array 256 :initial-element nil)))
    (mapcar (lambda (o) (modify-boxes boxes o)) (parse-input (uiop:read-file-string fname)))
    (calc-focusing-power boxes)))
