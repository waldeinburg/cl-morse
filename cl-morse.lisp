;;; === Utilities from Paul Graham, On Lisp ===
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
   (labels ((f (lst cur-ch-acc acc)
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
                           (#\space (f rest nil (cons #\space (acc-cur-ch))))
                           (#\_ (f rest nil (acc-cur-ch)))
                           (t (f rest (cons part cur-ch-acc) acc))))
                       (nreverse (acc-cur-ch))))))
     (f (coerce m-str 'list) nil nil))
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
  (if (cdr body)
      `(progn ,@(parse-morse body))
      (parse-morse (car body))))

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

(defgeneric to-morse (x))

;;; All methods return nil on unsupported characters and types.
(defmethod to-morse ((x t))
  nil)

(defmethod to-morse ((x character))
  (aif (char-to-morse x)
       (intern (concatenate 'string "C" it))))

(defmethod to-morse ((x string))
  (apply #'concatenate
    (cons 'string
          (labels ((f (lst in-word acc)
                      (if lst
                        (let ((ch (car lst))
                              (rest (cdr lst)))
                          (if (eql ch #\space)
                            (f rest nil (cons " " acc))
                            (aif (char-to-morse ch)
                                 (if in-word
                                   (f rest t (list* it "_" acc))
                                   (f rest t (cons it acc)))
                                 (return-from to-morse nil))))
                        (nreverse acc))))
            (f (coerce x 'list) nil nil)))))

(defmethod to-morse ((x number))
  (aif (to-morse (write-to-string x))
       (intern (concatenate 'string "N" it))))

(defmethod to-morse ((x symbol))
  (aif (to-morse (string-upcase (symbol-name x)))
       (intern it)))

;;; Notice that the type of NIL is symbol, not (empty) list.
;;; Thus, NIL will be converted correctly.
(defmethod to-morse ((x list))
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
