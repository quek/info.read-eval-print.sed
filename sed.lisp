(in-package :info.read-eval-print.sed)

(defvar *sed* nil)

(alexandria:define-constant +lf+ (format nil "~c" #\lf) :test 'equal)
(alexandria:define-constant +crlf+ (format nil "~c~c" #\cr #\lf) :test 'equal)

(defstruct sed
  (in *standard-input*)
  (out *standard-output*)
  hold-space
  pattern-space
  (line-numebr 0)
  (eol +lf+))

(define-symbol-macro *pattern-space* (sed-pattern-space *sed*))
(define-symbol-macro *line-number* (sed-line-numebr *sed*))
(define-symbol-macro *eol* (sed-eol *sed*))

(defun s (pattern replacement &rest options)
  (let ((pattern (ppcre:create-scanner pattern :case-insensitive-mode (member :i options))))
    (setf *pattern-space*
          (apply (if (member :g options)
                     #'ppcre:regex-replace-all
                     #'ppcre:regex-replace)
                 pattern *pattern-space* replacement nil))))

(defun c (text)
  (setf *pattern-space* text))

(defun i (text)
  (setf *pattern-space* (format nil "~a~a~a" text *eol* *pattern-space*)))

(defgeneric match-p (arg))

(defmethod match-p ((arg string))
  (ppcre:scan arg *pattern-space*))

(defmethod match-p ((arg integer))
  (= arg *line-number*))

(defmacro ? (address-or-address1-and-2 &body body)
  `(when (match-p ,address-or-address1-and-2)
     ,@body))

(defmacro sed ((&key (in *standard-input*)
                  (out *standard-output*)
                  (eol +lf+))
               &body body)
  (let* ((in-var (gensym "in"))
         (out-var (gensym "out"))
         (form `(let ((*sed* (make-sed :in ,in-var :out ,out-var :eol ,eol)))
                  (tagbody
                   :next
                     (unless (setf *pattern-space* (read-line (sed-in *sed*) nil))
                       (go :end))
                     (incf *line-number*)
                     (setf *pattern-space* (string-right-trim #(#\cr #\lf) *pattern-space*))
                     ,@body
                     (format (sed-out *sed*) "~a~a" *pattern-space* *eol*)
                     (go :next)
                   :end))))
    `(cond ((and (not (streamp ,in))
                 (not (streamp ,out)))
            (with-open-file (,in-var ,in)
              (with-open-file (,out-var ,out :direction :output :if-exists :supersede)
                ,form)))
           ((not (streamp ,in))
            (with-open-file (,in-var ,in)
              (let ((,out-var ,out))
                ,form)))
           ((not (streamp ,out))
            (let ((,in-var ,in))
              (with-open-file (,out-var ,out :direction :output :if-exists :supersede)
                ,form)))
           (t
            (let ((,in-var ,in)
                  (,out-var ,out))
              ,form)))))

#|
(sed (:in (merge-pathnames "a.html" (asdf:system-source-file :info.read-eval-print.sed)))
  (? 1
    (i "<!-- はじめの一歩 -->"))
  (? "/body"
     (c "---------------------------- end body"))
  (s "html" "FOOOO"))
|#
