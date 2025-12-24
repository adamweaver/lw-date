(in-package :cl-user)

(define-lw-system date ()
  (:system "lw-ppcre")
  (:file "package")
  (:file "date" :depends-on "package"))


