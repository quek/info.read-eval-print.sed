;;;; -*- Mode: LISP; -*-
(asdf:defsystem :info.read-eval-print.sed
  :version "0.0.0"
  :serial t
  :components ((:file "package")
               (:file "sed"))
  :depends-on (info.read-eval-print.series-ext
               cl-ppcre))
