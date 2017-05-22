(in-package #:dk.waldeinburg.cl-morse)

;;; === Utilities from Paul Graham, On Lisp ===
(eval-when (:compile-toplevel :load-toplevel :execute) ; used in macro
  (defun mklist (obj)
    (if (listp obj) obj (list obj))))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))
;;; ===========================================

;;; General utilities

(defmacro loopr (parms &body body)
  "Clojure-like tail recursion"
  (let ((parms-lst (mapcar #'mklist parms)))
    `(labels ((recur ,(mapcar #'car parms-lst)
                ,@body))
       (recur ,@(mapcar #'cadr parms-lst)))))

;;; While map can be used as a mapcar on sequences, it seems that the
;;; same does not exist for mapcan etc.
(defun map-seq (type map-fn fn seq)
  (coerce
   (funcall map-fn fn (coerce seq 'list))
   type))

(defun char->string (ch)
  (coerce (list ch) 'string))

;;; Morse

(defconstant +default-char-delim+ #\=)
(defconstant +default-word-delim-n+ 2)
(defconstant +default-word-delim+ #\Space)
(defvar +default-word-delim-str+
  (make-string +default-word-delim-n+ :initial-element +default-word-delim+))

(defparameter *morse2ascii*
  (let ((tbl (make-hash-table :test 'equal)))
    (flet ((fill-tbl (&rest pairs)
             (mapc (lambda (p)
                     (setf (gethash (cadr p) tbl)
                           (car p)))
                   (group pairs 2))))
      ;; List mainly from https://en.wikipedia.org/wiki/Morse_code
      ;; All foreign characters are removed because most are shared between
      ;; several characters.
      ;; A few cases (e.g. dot) cannot be represented without caracther escape
      ;; or vertical pipe, and this is applied to all cases for readability.
      (fill-tbl
       #\A ".-"
       #\B "-..."
       #\C "-.-."
       #\D "-.."
       #\E "."
       #\F "..-."
       #\G "--."
       #\H "...."
       #\I ".."
       #\J ".---"
       #\K "-.-"
       #\L ".-.."
       #\M "--"
       #\N "-."
       #\O "---"
       #\P ".--."
       #\Q "--.-"
       #\R ".-."
       #\S "..."
       #\T "-"
       #\U "..-"
       #\V "...-"
       #\W ".--"
       #\X "-..-"
       #\Y "-.--"
       #\Z "--.."
       #\0 "-----"
       #\1 ".----"
       #\2 "..---"
       #\3 "...--"
       #\4 "....-"
       #\5 "....."
       #\6 "-...."
       #\7 "--..."
       #\8 "---.."
       #\9 "----."
       #\. ".-.-.-"
       #\, "--..--"
       #\? "..--.."
       #\' ".----."
       #\! "-.-.--"
       #\/ "-..-."
       #\( "-.--."
       #\) "-.--.-"
       #\& ".-..."
       #\: "---..."
       #\; "-.-.-."
       #\= "-...-"
       #\+ ".-.-."
       #\- "-....-"
       #\_ "..--.-"
       #\" ".-..-."
       #\$ "...-..-"
       #\@ ".--.-."
       #\newline ".-.-" ; http://morsecode.scphillips.com/morse2.html
       #\* "..-.." ; okay, I made this one up
       #\Space " " ; for lookup when converting char
       )
      tbl)))

(defun char-from-morse (m-str)
  (gethash m-str *morse2ascii*))

(defun morse-char-p (ch)
  (or (eql ch #\.)
      (eql ch #\-)))

;;; Really just a helper for string-from-morse
;;; One space is added when multiple separators are encountered. It is
;;; thus possible to add one leading and trailing space.
(defun char-lst-from-morse (m-str)
  (loopr ((lst (coerce m-str 'list))
          (in-sep? nil) (in-word-delim? nil) ; in separator?
          cur-ch-acc acc) ; current char accumulator and accumulator
     (flet ((acc-cur-ch ()
              ;; If any morse characters are acummulated, translate
              ;; character and add to accumulator. Return accumulator.
              (if cur-ch-acc
                  (let* ((m (coerce (nreverse cur-ch-acc) 'string))
                         (ch (char-from-morse m)))
                    (if ch
                        (cons ch acc)
                        ;; Unknown morse code.
                        ;; Return nil and the unkown code
                        (return-from char-lst-from-morse
                          (values nil m))))
                  acc)))
       (if lst
           (let ((part (car lst))
                 (rest (cdr lst)))
             (if (morse-char-p part) 
                 ;; Part of character
                 (recur rest nil nil (cons part cur-ch-acc) acc)
                 (if (not in-sep?)
                     ;; End of character
                     (recur rest t nil nil (acc-cur-ch))
                     (if (not in-word-delim?)
                         ;; Multiple separators (end of word): add space
                         (recur rest t t nil (cons #\Space acc))
                         ;; Space already added (word separator is of
                         ;; arbitrary length)
                         (recur rest t t nil acc)))))
           ;; End of string. Add final acummulated morse characters.
           (nreverse (acc-cur-ch))))))

(defun string-from-morse (m-str)
  (multiple-value-bind (result error) (char-lst-from-morse m-str)
    (if result
        (coerce result 'string)
        (values nil error))))

(defun number-from-morse (m-str)
  (aif (string-from-morse m-str)
       (read-from-string it)))

(defun symbol-from-morse (m-str)
  (aif (string-from-morse m-str)
       (intern it)))

(defun parse-morse (m)
  (typecase m
    (null nil)
    (cons (cons (parse-morse (car m))
                (parse-morse (cdr m))))
    (string (string-from-morse m))
    (symbol
     (let* ((str (symbol-name m))
            (get-rest (lambda () (subseq str 1)))) ; lazy
       (case (char str 0)
         (#\C (char-from-morse (funcall get-rest)))
         (#\N (number-from-morse (funcall get-rest)))
         (otherwise (symbol-from-morse str)))))))

(defmacro morse-code (&body body)
  "Macro for writing Lisp morse code directly"
  (if (cdr body)
      `(progn ,@(parse-morse body))
      (parse-morse (car body))))


;;; And now a reader macro for the real morsy (is that a word, morsy?
;;; now it is) experience!
(defun make-morse-reader (morse-word
                          &optional
                            (end-sym (gensym)) ; do not end on nil by default
                            (recursive-p t))   ; for debugging
  (let ((morse-sym (intern (string-upcase (subseq morse-word 1))))
        (eof-sym (gensym)))
    ;; char parameters are optional to support both single and dispatching
    ;; macro characters as well as debugging without specifying any char.
    (lambda (stream &optional char char2)
      (declare (ignore char char2))
      (let ((sexp (read stream t nil recursive-p)))
        (if (eq sexp morse-sym)
            `(progn ,@(loop
                         for sexp = (read stream nil eof-sym
                                          ;; recursive-p t seems most correct,
                                          ;; but CLISP fails with simple-end-of-file
                                          #-clisp t
                                          #+clisp nil)
                         until (or (eq sexp eof-sym)
                                   (eq sexp end-sym))
                         collect (parse-morse sexp)))
            (parse-morse sexp))))))

(defun enable-morse-mode-fn (morse-word end-sym)
  "Helper for enable-morse-fn macro"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; Only run when the first macro character is set.
     (when (null *org-readtable*)
       ;; Make it possible to turn off.
       (setf *org-readtable* *readtable*)
       ;; Make local for this file. Cf. https://lisper.in/reader-macros
       (setf *readtable* (copy-readtable)))
     ,(let ((1st-char (char morse-word 0)))
           (if (eql 1st-char #\#)
               `(set-dispatch-macro-character #\# ,(char morse-word 1)
                                              ,(make-morse-reader
                                                (subseq morse-word 1)
                                                end-sym))
               `(set-macro-character ,1st-char
                                     ,(make-morse-reader morse-word end-sym))))))

;;; The first character of morse-word will be the macro character,
;;; interpreting the next sexp as morse. The first character is thus
;;; case sensitive.
;;; If # is the first character, a dispatch macro character is made.
;;; If the full morse-word is written, sexps are interpreted as morse
;;; until EOF or end-sym is encountered.
;;; End-sym will be the "end of work" morse code by default. The
;;; symbol is exported but if the package is not used one have to
;;; specify end-sym manually (or write cl-morse:...-.-).
;;; The terminator code of single telegrams (.-.-.) is not used
;;; because it is also the code for +.
;;; Cf.
;;; http://www.morsecode.nl/ITU-R-M%201677-International-Morse-code.pdf
;;; Morse-word and end-sym are converted to upper-case before being
;;; converted to symbols and are thus not case-sensitive.
;;; If morse-word is only a single character, full morse mode will
;;; still be available by wring e.g. M||.
;;; If morse-word is not provided, both MORSE and morse will be used.
(defmacro enable-morse-mode (&optional morse-word (end-sym '...-.-))
  "Enable morse reader macro"
  (if morse-word
      (enable-morse-mode-fn morse-word end-sym)
      `(progn
         ;; Allow MORSE, morse, Morse, mOrsE etc.
         ,(enable-morse-mode-fn "MORSE" end-sym)
         ,(enable-morse-mode-fn "morse" end-sym))))

(defvar *org-readtable* nil)

;;; The name includes m's, when using default morse mode it has to
;;; be called as:
;;; m(-.._.._..._.-_-..._.-.._._-....-_--_---_.-._..._._-....-_--_---_-.._.)
(defmacro disable-morse-mode ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *readtable* *org-readtable*)
     (setf *org-readtable* nil)
     '|-.-. .... .. -.-. -.- . -. -.-.--|))


;;; ASCII to morse. Of course, you don't need these in practice, do you?

(defparameter *ascii2morse*
  (let ((tbl (make-hash-table)))
    (maphash (lambda (key val)
               (setf (gethash val tbl) key))
             *morse2ascii*)
    tbl))

(defun char-to-morse (ch)
  (gethash (char-upcase ch) *ascii2morse*))

(defun string-to-morse (x char-delim word-delim-str)
  (let ((char-delim-str (char->string char-delim))
        (char-lst (coerce x 'list)))
    (let ((str-parts
           (loopr ((lst char-lst) in-word acc)
              (if lst
                  (let ((ch (car lst))
                        (rest (cdr lst)))
                    (if (eql ch #\Space)
                        (recur rest nil (cons word-delim-str acc))
                        (aif (char-to-morse ch)
                             (if in-word
                                 (recur rest t
                                        (list* it char-delim-str acc))
                                 (recur rest t
                                        (cons it acc)))
                             (return-from string-to-morse nil))))
                  (nreverse acc)))))
      (apply #'concatenate (cons 'string str-parts)))))

(defgeneric to-morse (x &optional char-delim word-delim-str))

(defmacro def-to-morse-method ((type) &body body)
  (let ((recursive? nil) ; set if normalize-to-morse-call does anything
        (to-morse-fns '(to-morse string-to-morse)))
    (labels ((normalize-to-morse-call (sexp)
               (if (consp sexp)
                   (cond
                     ((member (car sexp) to-morse-fns)
                      (setf recursive? t)
                      (append sexp '(char-delim word-delim-str)))
                     (t (mapcar #'normalize-to-morse-call sexp)))
                   sexp)))
      (let ((bodyn (mapcar #'normalize-to-morse-call body)))
        `(defmethod to-morse ((x ,type)
                              &optional
                                (char-delim +default-char-delim+)
                                (word-delim-str +default-word-delim-str+))
           ,@(if recursive?
                 bodyn
                 (cons '(declare (ignore char-delim word-delim-str)) body)))))))

;;; All methods return nil on unsupported characters and types.
(def-to-morse-method (t)
  nil)

(def-to-morse-method (character)
  (aif (char-to-morse x)
       (intern (concatenate 'string "C" it))))

(def-to-morse-method (string)
  (string-to-morse x))

(def-to-morse-method (number)
  (aif (string-to-morse (write-to-string x))
       (intern (concatenate 'string "N" it))))

(def-to-morse-method (symbol)
  (aif (string-to-morse (string-upcase (symbol-name x)))
       (intern it)))

;;; Notice that the type of NIL is symbol, not (empty) list.
;;; Thus, NIL will be converted correctly.
(def-to-morse-method (list)
  (mapcar (lambda (y)
            (aif (to-morse y)
                 it
                 (return-from to-morse nil)))
          x))

;;; For convenience, instead of using (to-morse '<expr>) on each expression.
(defmacro morsify-with ((char-delim
                         &optional (word-delim-str +default-word-delim-str+))
                        &body body)
  `(values ,@(mapcar (lambda (expr)
                       `',(to-morse expr char-delim word-delim-str))
                     body)))

;;; Shorthand for calling morsify-with with default values.
(defmacro morsify (&body body)
  `(morsify-with (,+default-char-delim+) ,@body))

;;; And finally some "normal" converters for everyday use.
(defun string->morse (str &optional (word-delim-n +default-word-delim-n+))
  (string-to-morse str
                   #\Space
                   (make-string word-delim-n :initial-element #\Space)))

(defun morse->string (str)
  (string-from-morse str))
