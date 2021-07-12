(uiop:define-package :icfpc2021/utils
    (:use #:cl)
  (:import-from #:cl-ppcre
		#:scan-to-strings)
  (:import-from #:yason)
  (:export #:problem-id-from-filename
	   #:dir-pathname
	   #:save-json
	   #:solution-file-name))

(in-package :icfpc2021/utils)

(defun problem-id-from-filename (file)
  (scan-to-strings "[0-9]+" (pathname-name file)))

(defun dir-pathname (sys-relative-path)
  (ensure-directories-exist
   (asdf:system-relative-pathname :icfpc2021 sys-relative-path)))

(defun solution-file-name (problem-id solutions-dir)
  (format nil "~A/solution-~A.json"
	  solutions-dir
	  problem-id))

(defmethod yason:encode ((x symbol) &optional stream)
  (yason:encode (format nil "~A" x) stream))

(defun save-json (ht filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
    (yason:encode ht stream)))
