(defpackage :com.gigamonkeys.id3v2
  (:use :common-lisp
        :com.gigamonkeys.binary-data
        :com.gigamonkeys.pathnames)
  (:export :read-id3 :mp3-p :id3-p :album :composer :genre
           :encoding-program :artist :part-of-set :track
           :song :year :size :translated-genre))


(define-binary-type unsigned-integer (bytes)
  (:reader (in)
           (loop with value = 0
                 for low-bit downfrom (* 8 (1- bytes)) to 0 by 8 do
                   (setf (ldb (byte 8 low-bit) value) (read-byte in))
                 finally (return value)))
  (:writer (out value)
           (loop for low-bit downfrom (* 8 (1- bytes)) to 0 by 8
                 do (write-byte (ldb (byte 8 low-bit) value) out))))

(define-binary-type u1 () (unsigned-integer :bytes 1))
(define-binary-type u2 () (unsigned-integer :bytes 2))
(define-binary-type u3 () (unsigned-integer :bytes 3))
(define-binary-type u4 () (unsigned-integer :bytes 4))


(define-binary-type unsigned-integer (bytes bits-per-byte)
  (:reader (in)
           (loop with value = 0
                 for low-bit downfrom (* bits-per-byte (1- bytes)) to 0
                 by bits-per-byte do (setf (ldb (byte bits-per-byte low-bit) value)
                                           (read-byte in))
                 finally (return value)))
  (:writer (out value)
           (loop for low-bit downfrom (* bits-per-byte (1- bytes)) to 0
                 by bits-per-byte do (write-byte (ldb (byte bits-per-byte low-bit)
                                                      value) out))))

(define-binary-type id3-tag-size () (unsigned-integer :bytes 4 :bits-per-byte 7))
