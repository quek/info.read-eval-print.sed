(cl:in-package :cl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (quicklisp:quickload :info.read-eval-print.sed)
  (quicklisp:quickload :trivial-shell))

(info.read-eval-print.series-ext:sdefpackage :info.read-eval-print.sed.test
                                             (:use :cl :info.read-eval-print.sed
                                                   :trivial-shell))

(cl:in-package :info.read-eval-print.sed.test)

(defparameter *text*
  "<html>
	<head>
		<title>テスト</title>
	</head>
	<body>
		<h1>その種</h1>
		<h2>ひつじ</h2>
		<p>もこもこ</p>
		<h2>ばく</h2>
		<p>はらまき</p>
	</body>
</html>
")

(defun ssed (expression)
 (with-input-from-string (in *text*)
   (trivial-shell:shell-command (format nil "sed -e '~a'" expression) :input in)))

(defmacro lsed (&body expression)
  (alexandria:with-gensyms (in out)
    `(with-output-to-string (,out)
       (with-input-from-string (,in *text*)
         (sed (:in ,in :out ,out)
           ,@expression)))))

(defmacro sl-test (ssed &rest lsed)
  `(progn
     (format *terminal-io* "~&~a" ,ssed)
     (assert (string= (ssed ,ssed)
                      (lsed ,@lsed))
             nil
             "~&****expected****~%~a~%****actual****~%~a"
             (ssed ,ssed)
             (lsed ,@lsed))))

(sl-test "s/html/foo/;s/body/AAA/"
         (s "html" "foo")
         (s "body" "AAA"))

(sl-test "1ihello" (? 1 (i "hello")))

(sl-test "$aend" (? :$ (a "end")))

(let ((*text* "a b c
1 2 3"))
  (assert (string= "b
2
" (lsed (s ".*" ($ 2))))))
