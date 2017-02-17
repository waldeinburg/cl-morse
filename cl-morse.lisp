;;; === Utilities from Paul Graham, On Lisp ===
(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(eval-when (:compile-toplevel) ; used in macro for *morse2ascii*
  (defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                     (rec rest (cons (subseq source 0 n) acc))
                     (nreverse (cons source acc))))))
      (if source (rec source nil) nil))))

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

;;; Morse

(defparameter *char-delim* #\_)

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
       #\space " " ; for lookup when converting char
       )
      tbl)))

(defun char-from-morse (m-str)
  (gethash m-str *morse2ascii*))

(defun string-from-morse (m-str)
  (coerce
   (loopr ((lst (coerce m-str 'list)) cur-ch-acc acc)
      ;; Possibly add current chars to accumulation after word end.
      (flet ((acc-cur-ch ()
               (if cur-ch-acc
                   (let ((m (coerce
                             (nreverse cur-ch-acc) 'string)))
                     (cons
                      (aif (char-from-morse m)
                           it
                           ;; Unknown morse code.
                           ;; Return nil and the unkown code
                           (return-from string-from-morse
                             (values nil m)))
                      acc))
                   acc)))
        (if lst
            (let ((part (car lst))
                  (rest (cdr lst)))
              (case part
                (#\space (recur rest nil (cons #\space (acc-cur-ch))))
                (#\_ (recur rest nil (acc-cur-ch)))
                (t (recur rest (cons part cur-ch-acc) acc))))
            (nreverse (acc-cur-ch)))))
   'string))

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
;;; The start and end signals are chosen according to
;;; http://www.morsecode.nl/ITU-R-M%201677-International-Morse-code.pdf
(defun make-morse-reader (morse-word &optional end-symb)
  (let ((morse-symb (intern (subseq (string-upcase morse-word) 1))))
    ;; char parameters are optional to support both single and dispatching
    ;; macro characters as well as debugging without specifying any char.
    (lambda (stream &optional char char2)
      (declare (ignore char char2))
      (let ((sexp (read stream t nil t)))
        (if (and (symbolp sexp)
                 (eq sexp morse-symb))
            `(progn ,@(loopr (forms-acc)
                         (let ((sexp (read stream nil nil t)))
                           (if (or (null sexp)
                                   (eq sexp end-symb))
                               (nreverse forms-acc)
                               (recur (cons (parse-morse sexp)
                                            forms-acc))))))
            (parse-morse sexp))))))

(defun enable-morse-mode-fn (morse-word end-symb)
  "Helper for enable-morse-fn macro"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ;; Only run when the first macro character is set.
     (when (null *org-readtable*)
       ;; Make it possible to turn off.
       (setq *org-readtable* *readtable*)
       ;; Make local for this file. Cf. https://lisper.in/reader-macros
       (setq *readtable* (copy-readtable)))
     ,(let ((1st-char (char morse-word 0))
            (reader (make-morse-reader morse-word end-symb)))
           (if (eql 1st-char #\#)
               `(set-dispatch-macro-character #\# ,(char morse-word 1)
                                              ,reader)
               `(set-macro-character ,1st-char
                                     ,reader)))))

;;; The first character of morse-word will be the macro character,
;;; interpreting the next sexp as morse. The first character is thus
;;; case sensitive.
;;; If # is the first character, a dispatch macro character is made.
;;; If the full morse-word is written, sexps are interpreted as morse
;;; until EOF or end-symb is encountered.
;;; End-symb will be the "end of work" morse code by default.
;;; The terminator code of single telegrams (.-.-.) is not used
;;; because it is also the code for +.
;;; Morse-word and end-symb are converted to upper-case before being
;;; converted to symbols and are thus not case-sensitive.
;;; If morse-word is only a single character, full morse mode will
;;; still be available by wring e.g. M||.
;;; If morse-word is not provided, both MORSE and morse will be used.
(defmacro enable-morse-mode (&optional morse-word (end-symb '...-.-))
  "Enable morse reader macro"
  (cond
    (morse-word (enable-morse-mode-fn morse-word end-symb))
    (t `(progn
          ;; Allow MORSE, morse, Morse, mOrsE etc.
         ,(enable-morse-mode-fn "MORSE" end-symb)
         ,(enable-morse-mode-fn "morse" end-symb)))))

(defvar *org-readtable* nil)

;;; The name includes m's, when using default morse mode it has to
;;; be called as:
;;; m(-.._.._..._.-_-..._.-.._._-....-_--_---_.-._..._._-....-_--_---_-.._.)
(defmacro disable-morse-mode ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* *org-readtable*)
     (setq *org-readtable* nil)
     'chicken!))


;;; ASCII to morse. Of course, you don't need these in practice, do you?

(defparameter *ascii2morse*
  (let ((tbl (make-hash-table)))
    (with-hash-table-iterator (generator-fn *morse2ascii*)
      (loop
        (multiple-value-bind (any? key value) (generator-fn)
          (unless any? (return))
          (setf (gethash value tbl) key))))
    tbl))

(defun char-to-morse (ch)
  (gethash (char-upcase ch) *ascii2morse*))

(defun string-to-morse (x &optional char-delim)
  (let ((char-delim-str (coerce (list (or char-delim
                                          *char-delim*))
                                'string))
        (char-lst (coerce x 'list)))
    (apply #'concatenate
           (cons 'string
                 (loopr ((lst char-lst) in-word acc)
                    (if lst
                        (let ((ch (car lst))
                              (rest (cdr lst)))
                          (if (eql ch #\space)
                              (recur rest nil (cons " " acc))
                              (aif (char-to-morse ch)
                                   (if in-word
                                       (recur rest t
                                              (list* it char-delim-str acc))
                                       (recur rest t
                                              (cons it acc)))
                                   (return-from string-to-morse nil))))
                        (nreverse acc)))))))

(defgeneric to-morse (x &optional char-delim))

(defmacro def-to-morse-method ((type) &body body)
  (labels ((normalize-to-morse-call (body)
             (if (consp body)
                 (if (member (car body) '(to-morse string-to-morse))
                     (append body '(char-delim))
                     (mapcar #'normalize-to-morse-call body))
                 body)))
    (let ((bodyn (mapcar #'normalize-to-morse-call body)))
      `(defmethod to-morse ((x ,type) &optional char-delim)
         ,@(if (equal body bodyn)
               (cons '(declare (ignore char-delim)) body)
               bodyn)))))

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
(defmacro morsify (&body body)
  `(values ,@(mapcar (lambda (expr)
                       `(quote ,(to-morse expr)))
                     body)))

;;; And finally some "normal" converters
(defun string->morse (str &optional (word-sep-n 2))
  (let ((word-sep-lst (loop repeat word-sep-n
                         collecting #\space)))
    (map-seq 'string
             #'mapcan
             (lambda (ch)
               (case ch
                 (#\space (copy-list word-sep-lst))
                 (#\_ (list #\space))
                 (otherwise (list ch))))
             (string-to-morse str #\_))))

(defun morse->string (str)
  (string-from-morse
   (map-seq 'string
            #'mapcon
            (lambda (lst)
              (if (eql (first lst) #\space)
                  ;; Look ahead. One space means character separation,
                  ;; multiple mean word separation and should be
                  ;; replaced with only one space.
                  (if (eql (second lst) #\space)
                      (unless (eql (third lst) #\space)
                        (list #\space))
                      (list #\_))
                  (list (first lst))))
            str)))
