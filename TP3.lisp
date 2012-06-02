(defparameter *in* nil)

(setf *in* (open "jcac.pdf" :if-does-not-exist nil))

(when *in*
	(loop for line = (read-line *in* nil)
		while line do (format t "~a~%" line))
	(close *in*))
