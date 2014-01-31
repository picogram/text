;; http://en.wikipedia.org/wiki/Wavelet_Tree

(defun build-tree (text)
  (let* ((b* (multiple-value-bind
		   (c cardinarity) (boundary-value text)
	       (list c cardinarity)))
	 (b (car b*))
	 (cardinarity (cadr b*)))
    (when (<= cardinarity 1)
      (return-from build-tree b))
    (loop
       with v = (make-array 1 :element-type 'bit
			    :adjustable t :fill-pointer 0)
       with l = (make-array 1 :element-type 'character
			    :adjustable t :fill-pointer 0)
       with r = (make-array 1 :element-type 'character
			    :adjustable t :fill-pointer 0)
       finally (return (list v (build-tree l) (build-tree r)))
       for c across text
       do 
	 (vector-push-extend (if (char<= c b) 0 1) v)
	 (vector-push-extend c (if (char<= c b) l r)))))


(defun boundary-value (seq)
  (let ((seq* (make-array 1 :element-type 'character
			   :adjustable t :fill-pointer 0)))
    (loop with current-char = #\Nul
       for c across (stable-sort (copy-seq seq) #'char<=)
       do (when (not (equalp c current-char))
	    (vector-push-extend c seq*))
	 (setf current-char c))
    (multiple-value-bind (x y) (floor (length seq*) 2)
      (values (aref seq*
		    (if (= 0 y)
			(max 0 (- x 1))
			x))
	      (length seq*)))))
    

