(defpackage :com.gigamonkeys.web
  (:use :cl :net.aserve :com.gigamonkeys.html))


(in-package :com.gigamonkeys.web)


(defun random-number (request entity)
  (with-http-response (request entity :content-type "text/html")
    (with-http-body (request entity)
      (format
       (request-reply-stream request)
       "<html>~@
        <head><title>Random</title></head>~@
        <body>~@
<p>Random number: ~d</p>~@
</body>~@
</html>~@
"
       (random 1000)))))


(html)
