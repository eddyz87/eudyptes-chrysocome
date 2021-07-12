(uiop:define-package :icfpc2021/http
    (:use :cl)
  (:import-from :yason
                #:encode)
  (:import-from :drakma
                #:http-request)
  (:import-from :flexi-streams
                #:octets-to-string)
  (:import-from :alexandria
                #:plist-hash-table
                #:with-output-to-file)
  (:export #:get-problem
           #:post-solution
           #:download-problems
           ))

(in-package :icfpc2021/http)

(defvar *url-base* "https://poses.live")
(defvar *auth-token* (format nil "Bearer ~A" (sb-ext:posix-getenv "ICFP_API_KEY")))

(defun get-problem (id)
  (octets-to-string
   (http-request (format nil "~A/api/problems/~A" *url-base* id)
                 :additional-headers `(("Authorization" . ,*auth-token*)))))

(defun download-problems (output-dir &key (from 1) (to from))
  (loop :for id :from from :to to
     :do (with-output-to-file (file (format nil "~A/problem_~A.json" output-dir id))
           (format file (get-problem id)))))

(defun post-solution (id solution)
  (let ((stream (http-request (format nil "~A/api/problems/~A/solutions" *url-base* id)
			      :additional-headers `(("Authorization" . ,*auth-token*))
			      :method :post
			      :want-stream t
			      :content (with-output-to-string (stream)
					 (encode (plist-hash-table (list "vertices" solution))
						 stream)))))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (print (yason:parse stream :object-as :plist))))
