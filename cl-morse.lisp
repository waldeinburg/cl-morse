(defparameter *morse2ascii*
  (let ((tbl (make-hash-table :test 'eq)))
    (macrolet ((fill-tbl (&rest pairs)
                     ; The setf could more cleanly be constructed with mapcan
                     ; on Paul Graham's group, but it's not written in morse!
                     `(setf ,@(labels
                               ((f (lst acc)
                                   (if lst
                                     (f (cddr lst)
                                        (nconc (list `(gethash ',(cadr lst) tbl)
                                                     `,(car lst))
                                               acc))
                                     acc)))
                               (f pairs nil)))))
      ; List mainly from https://en.wikipedia.org/wiki/Morse_code
      ; All foreign characters are removed because most are shared between several characters.
      ; A few cases (e.g. dot) cannot be represented without caracther escape or vertical pipe,
      ; and this is applied to all cases for readability.
      (fill-tbl
       #\A |.-|
       #\B |-...|
       #\C |-.-.|
       #\D |-..|
       #\E |.|
       #\F |..-.|
       #\G |--.|
       #\H |....|
       #\I |..|
       #\J |.---|
       #\K |-.-|
       #\L |.-..|
       #\M |--|
       #\N |-.|
       #\O |---|
       #\P |.--.|
       #\Q |--.-|
       #\R |.-.|
       #\S |...|
       #\T |-|
       #\U |..-|
       #\V |...-|
       #\W |.--|
       #\X |-..-|
       #\Y |-.--|
       #\Z |--..|
       #\0 |-----|
       #\1 |.----|
       #\2 |..---|
       #\3 |...--|
       #\4 |....-|
       #\5 |.....|
       #\6 |-....|
       #\7 |--...|
       #\8 |---..|
       #\9 |----.|
       #\. |.-.-.-|
       #\, |--..--|
       #\? |..--..|
       #\' |.----.|
       #\! |-.-.--|
       #\/ |-..-.|
       #\( |-.--.|
       #\) |-.--.-|
       #\& |.-...|
       #\: |---...|
       #\; |-.-.-.|
       #\= |-...-|
       #\+ |.-.-.|
       #\- |-....-|
       #\_ |..--.-|
       #\" |.-..-.|
       #\$ |...-..-|
       #\@ |.--.-.|
       #\newline |.-.-| ; http://morsecode.scphillips.com/morse2.html
       #\* |..-..| ; okay, I made this one up
       )
      tbl)))

(defun char-from-morse (m-ch)
  (gethash m-ch *morse2ascii*))

(defun char-from-morse-str (m-ch-str)
  (char-from-morse (intern m-ch-str)))

(defun string-from-morse (m-str)
  (coerce
   (labels ((f (lst cur-ch-acc acc)
               (flet ((acc-cur-ch ()
                                  (if cur-ch-acc
                                    (cons
                                     (char-from-morse-str
                                      (coerce (nreverse cur-ch-acc) 'string))
                                     acc)
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

(defun symbol-from-morse (m-str)
  (intern (string-from-morse m-str)))

(defparameter *ascii2morse*
  (let ((tbl (make-hash-table)))
    (with-hash-table-iterator (generator-fn *morse2ascii*)
      (loop
        (multiple-value-bind (any? key value) (generator-fn)
          (unless any? (return))
          (setf (gethash value tbl) key))))
    tbl))

(defgeneric to-morse (x))

(defmethod to-morse ((x t))
  (values x nil))

;;; Notice that when a character is converted, it is impossible to distinguish
;;; from a single character symbol. Use
;;; (-.-._...._.-_.-._.-_-.-._-_._.-. (--.-_..-_---_-_. <symbol>))
;;; to use characters in morse code.
(defmethod to-morse ((x character))
  (multiple-value-bind (m p) (gethash (char-upcase x) *ascii2morse*)
    (if p
      (values m t)
      (values x nil))))

(defmethod to-morse ((x string))
  (block nil
    (values
     (apply #'concatenate
       (cons 'string
             (labels ((f (lst in-word acc)
                         (if lst
                           (let ((ch (car lst))
                                 (rest (cdr lst)))
                             (if (eql ch #\space)
                               (f rest nil (cons " " acc))
                               (multiple-value-bind (m p) (to-morse ch)
                                 (if p
                                   (let ((m-str (symbol-name m)))
                                     (if in-word
                                       (f rest t (list* m-str "_" acc))
                                       (f rest t (cons m-str acc))))
                                   (return (values x nil))))))
                           (nreverse acc))))
               (f (coerce x 'list) nil nil))))
     t)))

; For types that are converted to string and returned as symbol
(defmacro define-stringable-to-morse (type str-conv-expr)
  `(defmethod to-morse ((x ,type))
     (multiple-value-bind (m p) (to-morse ,str-conv-expr)
       (if p
         (values (intern m) t) ; return as symbol
         (values x nil)))))

(define-stringable-to-morse number
  (write-to-string x))

(define-stringable-to-morse symbol
  (string-upcase (symbol-name x)))

(defmethod to-morse ((x list))
  (mapcar #'to-morse x))

(defmacro morsify (&body body)
  `',@(to-morse body))
