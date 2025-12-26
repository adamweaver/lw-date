(in-package :cl-user)

(define-lw-system lw-date ()
  (:system "lw-ppcre")
  (:file "package")
  (:file "date" :depends-on "package"))


