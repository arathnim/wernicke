;; (author *this-code*) => (:name "Dylan Ball" :email "Arathnim@gmail.com")

(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))
;; (declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(ql:quickload '(alexandria iterate anaphora) :silent t)
(defpackage wernicke (:use cl alexandria iterate anaphora))
(defpackage wernicke-parsers)

(in-package wernicke)

(declaim (string *source*))
(defvar *source* "")

(declaim (fixnum *source-length*))
(defvar *source-length* 0)

(declaim (fixnum *index*))
(defvar *index* 0)

;; error type and functions

(declaim (inline error?))
(defstruct (parser-error (:conc-name error-) (:predicate error?))
  (index 0 :type fixnum) data)

(defmacro succeed (value offset)
  `(progn (incf *index* ,offset) ,value))

(defmacro fail (data &optional (index *index*))
  `(make-parser-error :data ,data :index ,index))

(defmacro on-success (exp &rest body)
   (with-gensyms (r)
      `(let ((,r ,exp))
         (if (error? ,r)
             ,r
             (progn ,@body)))))

(defmacro on-failure (exp &rest body)
   (with-gensyms (r)
      `(let ((,r ,exp))
         (if (error? ,r)
             (progn ,@body)
             ,r))))

;; 'interpreted' parsers, for dynamic calling at runtime
;; used as a fallback and for prototyping new parsers
(defparameter interpreted-parsers (make-hash-table :test #'equalp))

;; parsers that serve as the first level of compilation,
;; generates code to be inserted by a macro
(defparameter compiled-parsers (make-hash-table :test #'equalp))

(defmacro define-internal-parser (name ll &rest body)
  `(setf (gethash ',name interpreted-parsers)
         (defun ,name ,ll ,@body)))

(defmacro define-compiled-parser (name ll &rest body)
  `(setf (gethash ',name compiled-parsers)
         (lambda ,ll ,@body)))

(defmacro run-parser (data)
	(if (and (listp data) (eq (car data) 'quote))
		 (compile-parser (second data))
		 (with-gensyms (foo)
			`(let ((,foo ,data))
				 (apply (car ,foo) (cdr ,foo))))))

(defun compile-parser (exp)
  (aif (and (listp exp) (gethash (car exp) compiled-parsers))
       (apply it (cdr exp))
       (error "unexpected form: ~a" exp)))

(defmacro parse (string form)
  `(let* ((*source* ,string)
          (*source-length*
             ,(if (stringp string) (length string) '(length *source*)))
          (*index* 0))
          (run-parser ,form)))

(defparameter user-parsers (make-hash-table :test #'equalp))

(defmacro defparser (name ll &rest body)
  `(setf (gethash ',name user-parsers)
         (list ,ll ,body)))

;; chr      ~ matches a single character
;; str      ~ explicitly matches a string
;; choice   ~ returns the first parser that succeeds
;; seq      ~ matches each form sequentially
;; any      ~ matches zero or more of the form
;; many     ~ matches one or more of the form
;; times    ~ parses x occurances of y
;; try      ~ returns a parsing result, no effect on the normal parser stack
;; optional ~ tries x, parses it on success, succeeds either way
;; sep      ~ seperates by repeated matching. mostly magic
;; one-of   ~ matches one character from the given string
;; none-of  ~ matches only if none of the chars in the string match
;; range    ~ matches a character numerically in a given range

(defmacro test-remaining (n)
  `(>= *source-length* (+ *index* ,n)))

;; chr

(define-internal-parser chr (char)
   (if (test-remaining 1)
       (if (eql (char *source* *index*) char)
           (succeed char 1)
           (fail (string char)))
       (fail (string char))))

(define-compiled-parser chr (char)
  (with-gensyms (genchar)
   `(let ((,genchar ,char))
      (declare (character ,genchar))
      (if (test-remaining 1)
         (if (eql (char *source* *index*) ,genchar)
             (succeed ,genchar 1)
             (fail (string ,genchar)))
         (fail (string ,genchar))))))

;; str

(define-internal-parser str (string)
   (iter (for char in-string string)
         (for parse-result = (run-parser `(chr ,char)))
         (on-failure parse-result (leave parse-result))
         (finally (return string))))

(define-compiled-parser str (string)
   (with-gensyms (genstring length)
     `(let* ((,genstring ,string) (,length (length ,genstring)))
        (declare (string ,genstring) (fixnum ,length))
        (if (test-remaining ,length)
            (iter (for x from 0 below ,length)
                  (declare (fixnum x))
                  (unless (eql (char ,genstring x) (char *source* (+ *index* x)))
                    (leave (fail `(expected ,,genstring))))
                  (finally (return (succeed ,genstring ,length))))
            (fail ,genstring)))))

;; try

(define-internal-parser try (p)
   (let* ((*index* *index*))
      (run-parser p)))

(define-compiled-parser try (p)
  `(let* ((*index* *index*))
      ,(compile-parser p)))

;; any

(define-internal-parser any (p)
   (iter (for r = (run-parser p))
         (until (error? r))
         (collect r into result)
         (finally (return result))))

(define-compiled-parser any (p)
   (with-gensyms (result list)
      `(iter (for ,result = ,(compile-parser p))
             (until (error? ,result))
             (collect ,result into ,list)
             (finally (return ,list)))))

;; many

(define-internal-parser many (p)
   (iter (for r = (run-parser p))
         (until (error? r))
         (collect r into result)
         (finally (return (or result (fail 'many))))))

(define-compiled-parser many (p)
   (with-gensyms (result list)
      `(iter (for ,result = ,(compile-parser p))
             (until (error? ,result))
             (collect ,result into ,list)
             (finally (return (or ,list ,result))))))

;; choice

(define-internal-parser choice (&rest parsers)
   (iter (for parser in parsers)
         (for parser-result = (run-parser parser))
         (on-success parser-result (leave parser-result))
         (finally (return (fail 'choice)))))

(define-compiled-parser choice (p &rest rest)
	(if rest
	  (with-gensyms (result)
		`(let ((,result ,(compile-parser p)))
			(if (error? ,result)
				,(compile-parser `(choice ,@rest))
				,result)))	  
     (with-gensyms (result)
		`(let ((,result ,(compile-parser p)))
			(if (error? ,result)
				 (fail 'choice)
				,result)))))

;; times

(define-internal-parser times (n parser)
   (iter (repeat n)
         (for parse-result = (run-parser parser))
         (on-failure parse-result (leave parse-result))
         (collect parse-result into list)
         (finally (return list))))

(define-compiled-parser times (n parser)
   (iter (repeat n)
         (for parse-result = (run-parser parser))
         (on-failure parse-result (leave parse-result))
         (collect parse-result into list)
         (finally (return list))))

;; optional

(define-internal-parser optional (parser)
   (if (error? (run-parser (try parser)))
       nil
       (run-parser parser)))

