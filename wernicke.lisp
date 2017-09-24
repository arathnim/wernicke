;; (author this-code) => (:name "Dylan Ball" :email "Arathnim@gmail.com")

(declaim (optimize (speed 3) (safety 0) (debug 3) (space 0)))

(ql:quickload '(alexandria iterate anaphora) :silent t)

(defpackage wernicke 
   (:use cl alexandria iterate anaphora)
   (:export "DEFPARSER" "PARSE" "CHR" "STR" "ONE-OF" "NONE-OF" 
      "ANY" "MANY" "SEQ" "CHOICE" "RANGE" "TRY" "OPTIONAL" "TIMES"
      "*RUN-HOOKS*"))

(in-package wernicke)

;; data

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

(defmacro fail (data &optional (index '*index*))
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

;; parsers

(defparameter *parser-list* nil)

(defparameter *run-hooks* nil)

(defmacro define-parser (name ll &rest body)
  `(progn (defun ,name ,ll (lambda () ,@body))
          (push ',name *parser-list*)))

(defmacro parse (string form)
  `(let* ((*source* ,string)
          (*source-length* (length *source*))
          (*index* 0))
          (funcall ,form)))

(defmacro defparser (name ll exp)
   `(define-parser ,name ,ll (funcall ,exp)))

(declaim (inline run-parser))
(defun run-parser (p)
   (iter (for h in *run-hooks*) (setf p (funcall h p)))
   (funcall p))

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

(define-parser chr (char)
   (if (test-remaining 1)
       (if (eql (char *source* *index*) char)
           (succeed char 1)
           (fail (string char)))
       (fail (string char))))

(define-parser str (string)
   (iter (for char in-string string)
         (for parse-result = (run-parser (chr char)))
         (on-failure parse-result (leave parse-result))
         (finally (return string))))

(define-parser one-of (string)
   (if (test-remaining 1) 
       (iter (for char in-string string)
             (with c = (char *source* *index*))
             (if (eql c char)
                 (leave (succeed char 1)))
             (finally (return (fail string))))
       (fail string)))

(define-parser none-of (string)
   (if (test-remaining 1) 
       (iter (for char in-string string)
             (with c = (char *source* *index*))
             (if (eql c char)
                 (leave (fail string)))
             (finally (return (succeed c 1))))
       (fail string)))

(define-parser try (p)
   (let* ((*index* *index*))
      (run-parser p)))

(define-parser any (p)
   (iter (for r = (run-parser p))
         (until (error? r))
         (collect r into result)
         (finally (return result))))

(define-parser many (p)
   (iter (for r = (run-parser p))
         (until (error? r))
         (collect r into result)
         (finally (return (or result (fail 'many))))))

(define-parser choice (&rest parsers)
   (iter (for parser in parsers)
         (for parser-result = (run-parser parser))
         (on-success parser-result (leave parser-result))
         (finally (return (fail 'choice)))))

(define-parser seq (&rest parsers)
   (iter (for parser in parsers)
         (for parser-result = (run-parser parser))
         (on-failure parser-result (leave parser-result))
         (collect parser-result)))

(define-parser times (n parser)
   (iter (repeat n)
         (for parse-result = (run-parser parser))
         (on-failure parse-result (leave parse-result))
         (collect parse-result into list)
         (finally (return list))))

(define-parser optional (parser)
   (if (error? (run-parser (try parser)))
       nil
       (run-parser parser)))

(define-parser range (a b)
   (if (test-remaining 1)
       (iter (for c from (char-code a) to (char-code b))
             (with char = (char *source* *index*))
             (if (eql char (code-char c)) 
                 (leave (succeed char 1)))
             (finally (return (fail (list a b)))))
       (fail (list a b))))
