(uiop:define-package :icfpc2021/http
    (:use :cl)
  (:import-from :drakma
                #:http-request)
  (:import-from :flexi-streams
                #:octets-to-string)
  (:export #:get-problem
           #:post-solution))

(in-package :icfpc2021/http)

(defvar *url-base* "https://poses.live")
(defvar *auth-token* (format nil "Bearer ~A" (sb-ext:posix-getenv "ICFP_API_KEY")))

(defun get-problem (id)
  (octets-to-string
   (http-request (format nil "~A/api/problems/~A" *url-base* id)
                 :additional-headers `(("Authorization" . ,*auth-token*)))))

(defun post-solution (id solution)
  (http-request (format nil "~A/api/problems/~A/solutions" *url-base* id)
                :additional-headers `(("Authorization" . ,*auth-token*))
                :method :post
                :content solution))
