(declaim (optimize (debug 3)))

(defun expt2 (num)
  (floor (expt 2 (1- num))))

(defun points-of-line (line tally-func)
  (destructuring-bind (win nums)
      (uiop:split-string (subseq line (1+ (position #\: line))) :separator '(#\|))
    (flet ((numset (l) (remove-if #'uiop:emptyp (uiop:split-string l :separator '(#\  #\Newline)))))
      (funcall tally-func (length (intersection (numset win) (numset nums) :test #'string=))))))

(defun solve-part-1 (fname)
  (reduce #'+ (mapcar (lambda (l) (points-of-line l #'expt2)) (uiop:read-file-lines fname))))

(defun counts-and-games (fname)
  (mapcar (lambda (x) (cons 1 x)) (uiop:read-file-lines fname)))

(defun add-cards (rem-counts-and-games count-and-game)
  (loop :for i :from 0 :below (min (length rem-counts-and-games)
                                   (points-of-line (cdr count-and-game) #'identity))
        :do (incf (car (nth i rem-counts-and-games)) (car count-and-game))
        :finally (return rem-counts-and-games)))

(defun sum-cards (counts-and-games sum-so-far)
  (let* ((count-and-game (first counts-and-games))
         (sum (+ sum-so-far (car count-and-game))))
    (if (rest counts-and-games)
        (sum-cards (add-cards (rest counts-and-games) count-and-game) sum)
        sum)))

(defun solve-part-2 (fname)
  (sum-cards (counts-and-games fname) 0))
