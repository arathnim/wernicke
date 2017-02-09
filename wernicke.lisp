;; (author *this-code*) => ("Dylan Ball" "Arathnim@gmail.com")

(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0)))
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))
(ql:quickload '(alexandria iterate anaphora) :silent t)
(defpackage wernicke
   (:use cl alexandria iterate anaphora))

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
   type index args)

(defmacro pass (value n)
   `(progn (incf *index* ,n) ,value))

(declaim (inline fail))
(defun fail (type args &optional (index *index*))
   (make-parser-error :type type :args args :index index))

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

(defmacro define-parser (name ll &rest body)
  `(setf (gethash ',name interpreted-parsers)
         (lambda ,ll ,@body)))

(defmacro define-compiled-parser (name ll &rest body)
  `(setf (gethash ',name compiled-parsers)
         (lambda ,ll ,@body)))

(defun dynamic-run (exp)
  (aif (and (listp exp) (gethash (car exp) interpreted-parsers))
       (apply it (cdr exp))
       (error "unexpected form: ~a" exp)))

(defun compile-parser (exp)
  (aif (and (listp exp) (gethash (car exp) compiled-parsers))
       (apply it (cdr exp))
       (error "unexpected form: ~a" exp)))

(defmacro insert-compiled (exp)
   (compile-parser exp))

(defmacro parse (string form)
  `(let* ((*source* ,string)
          (*source-length* 
             ,(if (stringp string) (length string) '(length *source*)))
          (*index* 0))
          (dynamic-run ',form)))

(defmacro compiled-parse (string form)
  `(let* ((*source* ,string)
          (*source-length* 
             ,(if (stringp string) (length string) '(length *source*)))
          (*index* 0))
          (insert-compiled ,form)))

;; programmer options
;;  backtracking
;;  value-returning
;;  errors

(defvar *settings* '(:backtracking nil :value-returning t :errors t))

;; choice   ~ in the matching operation, returns the first form that consumes input
;; any      ~ matches zero or more of the next form
;; many     ~ matches one or more of the next form
;; one-of   ~ matches one character from the given string
;; none-of  ~ matches only if none of the chars in the string match
;; times    ~ parses x occurances of y
;; try      ~ returns a parsing result, no effect on the normal parser stack
;; optional ~ tries x, parses it on success, succeeds either way
;; seq      ~ matches each form sequentially, returns a list of forms or nil
;; str      ~ explicitly matches a string
;; sep      ~ seperates by repeated matching. mostly magic

(defmacro test-remaining (n)
  `(>= *source-length* (+ *index* ,n)))

;; char

(define-parser chr (char)
   (if (test-remaining 1)
       (if (eql (char *source* *index*) char)
           (pass char 1)
           (fail (string char) nil))
       (fail (string char) nil)))

(define-compiled-parser chr (char)
  (with-gensyms (genchar)
   `(let ((,genchar ,char))
      (declare (character ,genchar))
      (if (test-remaining 1)
         (if (eql (char *source* *index*) ,genchar)
             (pass ,genchar 1)
             (fail (string ,genchar) nil))
         (fail (string ,genchar) nil)))))

;; str

(define-parser str (string)
   (iter (for char in-string string)
         (for parse-result = (dynamic-run `(chr ,char)))
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
                    (leave (fail 'expected (list ,genstring))))
                  (finally (return (pass ,genstring ,length))))
            (fail ,genstring nil)))))

;; try

(define-parser try (p)
   (let* ((*index* *index*))
      (dynamic-run p)))

;; any

(define-parser any (p)
   (iter (for r = (dynamic-run p))
         (until (error? r))
         (collect r into result)
         (finally (return (merge-characters result)))))

;; many

(define-parser many (p)
   (iter (for r = (dynamic-run p))
         (until (error? r))
         (collect r into result)
         (finally (return (if result (merge-characters result) (fail 'many nil))))))

(define-parser choice (&rest parsers)
   (iter (for parser in parsers)
         (for parser-result = (dynamic-run parser))
         (on-success parser-result (leave parser-result))
         (finally (return (fail 'choice nil)))))

(define-parser times (n parser)
   (iter (repeat n)
         (for parse-result = (dynamic-run parser))
         (on-failure parse-result (leave parse-result))
         (collect parse-result into list)
         (finally (return (merge-characters list)))))

(define-parser optional (parser)
   (if (error? (dynamic-run (try parser)))
       nil
       (run parser)))
