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
  (field-separator "\\s+")
  ($ nil)
  (n nil)
  (numomo (make-hash-table)))

(define-symbol-macro *pattern-space* (sed-pattern-space *sed*))
(define-symbol-macro *hold-space* (sed-hold-space *sed*))
(define-symbol-macro *line-number* (sed-line-numebr *sed*))
(define-symbol-macro *eol* (sed-eol *sed*))

(defun do-before-output (sed)
  (collect-ignore (funcall (scan 'list (sed-before-output sed)))))

(defun do-after-output (sed)
  (collect-ignore (funcall (scan 'list (sed-after-output sed)))))

(defun read-next (sed)
  (let ((line (read-line (sed-in sed) nil)))
    (if line
        (progn
          (incf *line-number*)
          (setf (sed-$ sed) (ppcre:split (sed-field-separator sed) line)))
        (progn
          (setf (sed-$ sed) nil)))
    (setf (sed-pattern-space sed) line)))

(defun $ (n)
  (if (zerop n)
      *pattern-space*
      (nth (1- n) (sed-$ *sed*))))

(defun a (text)
  (push (lambda ()
          (format (sed-out *sed*) "~a~a" text *eol*))
        (sed-after-output *sed*)))

(defun c (text)
  (setf *pattern-space* text))

(defun d ()
  (throw :next nil))

(defun g ()
  (setf *pattern-space* *hold-space*))

(defun g* ()
  (setf *pattern-space* (format nil "~a~a~a" *pattern-space* *eol* *hold-space*)))

(defun h ()
  (setf *hold-space* *pattern-space*))

(defun h* ()
  (setf *hold-space* (format nil "~a~a~a" *hold-space* *eol* *pattern-space*)))

(defun i (text)
  (push (lambda ()
          (format (sed-out *sed*) "~a~a" text *eol*))
        (sed-before-output *sed*)))

(defun n ()
  (i *pattern-space*)
  (setf *pattern-space* (read-next *sed*)))

(defun n* ()
  (setf *pattern-space* (format nil "~a~a~a" *pattern-space* *eol*
                                (or (read-next *sed*) ""))))

(defun p ()
  (format (sed-out *sed*) "~a~a" *pattern-space* *eol*))

(defun q ()
  (unless (sed-n *sed*) (p))
  (throw :quit nil))

(defun s (pattern replacement &rest options)
  (let ((pattern (ppcre:create-scanner pattern :case-insensitive-mode (member :i options))))
    (setf *pattern-space*
          (apply (if (member :g options)
                     #'ppcre:regex-replace-all
                     #'ppcre:regex-replace)
                 pattern *pattern-space* replacement nil))))

(defun x ()
  (rotatef *pattern-space* *hold-space*))

(defgeneric match-p (arg))

(defmethod match-p ((arg string))
  (ppcre:scan arg *pattern-space*))

(defmethod match-p ((arg integer))
  (= arg *line-number*))

(defmethod match-p ((arg (eql :$)))
  (not (peek-char nil (sed-in *sed*) nil)))

(defmacro ? (address-or-address1-and-2 &body body)
  `(when (match-p ,address-or-address1-and-2)
     ,@body))

(defmacro ?? (from to &body body)
  "/from/,/to/body
form から to までのテキストブロックに対して body を実行"
  (alexandria:with-gensyms (line)
    `(when (match-p ,from)
       (loop for ,line = (read-next *sed*)
             while ,line
             do (setf *pattern-space* (format nil "~a~a~a" *pattern-space* *eol* ,line))
             if (let ((*pattern-space* ,line))
                  (match-p ,to))
               do ,@body
                  (loop-finish)))))

(defmacro ??* (from to &body body)
  "/from/,/to/{body}
form から to までそれぞれの行に対して body を実行"
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
                  (input-external-format :default)
                  out
                  (output-external-format input-external-format)
                  (eol +lf+)
                  (field-separator "\\s+")
                  n
                  i)
               &body body)
  (alexandria:once-only (in out eol i n)
    (let ((in-var (gensym "in"))
          (out-var (gensym "out")))
      `(progn
         (cond ((eq ,i t)
                (setf ,out (swank-backend::temp-file-name)))
               ((stringp ,i)
                (setf ,out (make-pathname :defaults ,in :type ,i))))
         (with-open-stream (,in-var (if (streamp ,in)
                                        ,in
                                        (open ,in :external-format ,input-external-format)))
           (with-open-stream (,out-var (cond ((null ,out)
                                              *standard-output*)
                                             ((streamp ,out)
                                              ,out)
                                             (t
                                              (open ,out :direction :output
                                                         :if-exists :supersede
                                                         :external-format ,output-external-format))))
             (catch :quit
               (prog ((*sed* (make-sed :in ,in-var :out ,out-var :eol ,eol :n ,n,
                                       :field-separator ,field-separator)))
                :next
                  (unless (read-next *sed*)
                    (go :end))
                  (setf (sed-before-output *sed*) nil
                        (sed-after-output *sed*) nil)
                  (setf *pattern-space* (string-right-trim #(#\cr #\lf) *pattern-space*))
                  (catch :next
                    (unwind-protect
                         (progn
                           (unwind-protect
                                (progn ,@body)
                             (do-before-output *sed*))
                           (unless (sed-n *sed*)
                             (p)))
                      (do-after-output *sed*)))
                  (go :next)
                :end))))
         (when (eq ,i t)
           (cl-fad:copy-file ,out ,in :overwrite t)
           (delete-file ,out))))))
