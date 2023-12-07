(declaim (optimize (debug 3)))

(defparameter *card-vals* "AKQJT98765432")

(defparameter *hand-types*
  '((5) (4 1) (3 2) (3 1 1) (2 2 1) (2 1 1 1) (1 1 1 1 1)))

(defun hand-type (hand)
  (sort (loop :for c :across *card-vals* :for n := (count c hand) :when (> n 0) :collect n) #'>))

(defun parse-input (fname)
  (loop :for line :in (uiop:read-file-lines fname) :for count :from 1
        :for (hand bid-str) := (uiop:split-string line)
        :collect (cons hand (parse-integer bid-str)) :into hnbs
        :finally (return (cons hnbs count))))

(defun group-hands-n-bids (hands-and-bids)
  (let ((type-to-hnb (make-hash-table :test #'equalp)))
    (loop :for hnb :in hands-and-bids
          :for type := (funcall *hand-type-fn* (car hnb))
          :do (push hnb (gethash type type-to-hnb)))
    type-to-hnb))

(defun cmp-hands (h1 h2)
  (loop :for c1 :across h1 :for c2 :across h2
        :for v1 := (position c1 *card-vals*) :for v2 := (position c2 *card-vals*)
        :when (/= v1 v2) :return (< v1 v2)))

(defun sum-hands-winnings (hands-bids rank-from)
  (loop :for rank :downfrom rank-from :to 1 :for hnb :in hands-bids
        :sum (* (cdr hnb) rank) :into winnings
        :finally (return (list winnings rank))))

(defun count-ranks (type-to-hnb max-rank)
  (loop :for type :in *hand-types*
        :for hands-bids := (sort (copy-list (gethash type type-to-hnb)) #'cmp-hands :key #'car)
        :for (winnings rem-rank) := (sum-hands-winnings hands-bids (or rem-rank max-rank))
        :sum winnings))

(defun solve-part-1 (fname)
  (let ((*hand-type-fn* #'hand-type)))
  (destructuring-bind (hnbs . max-rank) (parse-input fname)
    (count-ranks (group-hands-n-bids hnbs) max-rank)))

(defun hand-type-j (hand)
  (let ((maybe-non-type
          (sort (loop :for c :across (remove #\J *card-vals*)
                      :for n := (count c hand) :when (> n 0) :collect n)
                #'>))
        (num-js (count #\J hand)))
    (cond ((= num-js 0) maybe-non-type)
          ((= num-js 5) '(5))
          (t (incf (car maybe-non-type) num-js) maybe-non-type))))

(defun solve-part-2 (fname)
  (let ((*hand-type-fn* #'hand-type-j)
        (*card-vals* "AKQT98765432J"))
    (destructuring-bind (hnbs . max-rank) (parse-input fname)
      (count-ranks (group-hands-n-bids hnbs) max-rank))))
