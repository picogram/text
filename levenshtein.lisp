
(defun levenshtein-distance (a b)
  (let ((len-a (length a))
	(len-b (length b)))
  (when (equalp a b)
    (return-from levenshtein-distance 0))
  (when (= len-a 0)
    (return-from levenshtein-distance len-b))
  (when (= len-b 0)
    (return-from levenshtein-distance len-a))
  (let ((v0 (make-array (1+ len-b) :element-type 'integer))
	(v1 (make-array (1+ len-b) :element-type 'integer)))
    (loop for i from 0 below (length v0)
       do (setf (aref v0 i) i))
    (loop for i from 0 below len-a
       do (setf (aref v1 0) (1+ i))
	 (loop for j from 0 below len-b
	      do (setf (aref v1 (1+ j)) (min (1+ (aref v1 j))
					     (aref v0 (1+ j))
					     (+ (aref v0 j)
						(if (equalp (aref a i) (aref b j))
						    0
						    1)))))
	 (loop for j from 0 below (length v0)
	    do (setf (aref v0 j) (aref v1 j))))
    (aref v1 len-b)))))
      