(in-package #:pg-text)

(defun rank (char seq end)
  (let ((count 0))
    (loop
	 for i from 0 below end
       do (when (equalp char (nth i seq))
	    (incf count)))
    count))

(defun make-suffix-array (text)
  (let* ((text* (concatenate 'string text '(#\Nul)))
	 (suffix (stable-sort (loop for i from 0 below (length text*)
		      collect (list (concatenate 'string (subseq text i) '(#\Nul)) i)) #'string-lessp :key #'car))
	 (suffix-array (make-array (length text*) :element-type 'integer)))
    (loop for x from 0 below (length suffix)
       do
	 (setf (aref suffix-array x) (cadr (nth x suffix))))
    suffix-array))


(defun bwt (text)
  (let* ((p 0)
	 (suffix-array (make-suffix-array text))
	 (transformed (make-array (length suffix-array) :element-type 'character)))
    (loop for i from 0 below (length suffix-array)
	 do (if (= (aref suffix-array i) 0)
		(progn
		  (setf (aref transformed i) #\Nul)
		  (setf p i))
		(setf (aref transformed i)
		      (aref text (- (aref suffix-array i) 1)))))
    (values transformed p)))
	

