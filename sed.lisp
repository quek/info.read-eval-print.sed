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
  (eol +lf+)
  (before-output nil)
  (after-output nil)
  (numomo (make-hash-table)))

(defun do-before-output (sed)
  (collect-ignore (funcall (scan 'list (sed-before-output sed)))))

(defun do-after-output (sed)
  (collect-ignore (funcall (scan 'list (sed-after-output sed)))))

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

(defun d ()
  (throw :next nil))

(defun i (text)
  (push (lambda ()
          (format (sed-out *sed*) "~a~a" text *eol*))
        (sed-before-output *sed*)))

(defun a (text)
  (push (lambda ()
          (format (sed-out *sed*) "~a~a" text *eol*))
        (sed-after-output *sed*)))

(defgeneric match-p (arg))

(defmethod match-p ((arg string))
  (ppcre:scan arg *pattern-space*))

(defmethod match-p ((arg integer))
  (= arg *line-number*))

(defmacro ? (address-or-address1-and-2 &body body)
  `(when (match-p ,address-or-address1-and-2)
     ,@body))

(defmacro ?? (from to &body body)
  (alexandria:with-gensyms (in-match-p)
    `(let ((,in-match-p nil))
       (when (if (gethash ,in-match-p (sed-numomo *sed*))
                 (progn
                   (when (match-p ,to)
                     (setf (gethash ,in-match-p (sed-numomo *sed*)) nil))
                   t)
                 (when (match-p ,from)
                   (setf (gethash ,in-match-p (sed-numomo *sed*)) t)))
         ,@body))))

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
                     (setf (sed-before-output *sed*) nil
                           (sed-after-output *sed*) nil)
                     (setf *pattern-space* (string-right-trim #(#\cr #\lf) *pattern-space*))
                     (catch :next
                       (unwind-protect
                            (progn
                              (unwind-protect
                                   (progn ,@body)
                                (do-before-output *sed*))
                              (format (sed-out *sed*) "~a~a" *pattern-space* *eol*))
                         (do-after-output *sed*)))
                     (go :next)
                   :end))))
    ;;; ↓どうにかなりませんか?
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
  (s "html" "FOOOO")
  (? "<title>"
    (a "だいめい"))
  (? "<title>"
     (d)))

(sed (:in (merge-pathnames "a.html" (asdf:system-source-file :info.read-eval-print.sed)))
  (?? "h2" "body"
    (i "bababa")))
|#
