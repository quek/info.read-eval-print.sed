(in-package :info.read-eval-print.sed)

(defvar *sed* nil)

(defstruct sed
  (in *standard-input*)
  (out *standard-output*)
  hold-space
  pattern-space
  (line-numebr 0))

(define-symbol-macro *pattern-space* (sed-pattern-space *sed*))

(defun s (pattern replacement &rest options)
  (let ((pattern (ppcre:create-scanner pattern :case-insensitive-mode (member :i options))))
    (setf *pattern-space*
          (apply (if (member :g options)
                     #'ppcre:regex-replace-all
                     #'ppcre:regex-replace)
                 pattern *pattern-space* replacement nil))))

(defun c (text)
  (setf *pattern-space* text))

(defmacro ? (address-or-address1-and-2 &body body)
  `(when (ppcre:scan ,address-or-address1-and-2 *pattern-space*)
     ,@body))

(defmacro sed ((&key (in *standard-input*) (out *standard-output*)) &body body)
  (let* ((in-var (gensym "in"))
         (out-var (gensym "out"))
         (form `(let ((*sed* (make-sed :in ,in-var :out ,out-var)))
                  (tagbody
                   :next
                     (unless (setf *pattern-space* (read-line (sed-in *sed*) nil))
                       (go :end))
                     ,@body
                     (write-line *pattern-space* (sed-out *sed*))
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

(sed (:in "/usr/share/doc/hyperspec/Body/01_.htm")
  (? "/BODY"
     (c "---------------------------- end body"))
  (s "HTML" "FOOOO"))
