(asdf:operate 'asdf:load-op 'cl-morse)
(use-package 'cl-morse)

(defmacro print-each (&body body)
  `(progn ,@(mapcar (lambda (sexp) `(print ,sexp)) body)))

(defmacro print-2-values (sexp)
  `(multiple-value-bind (a b) ,sexp
     (print a)
     (print b)))

(defun heading (s)
  (format t "~&=== ~a ===" s))


;;; Different types handled.
(heading "parse-morse, basic")
(print-each
  ;; FOO
  (parse-morse '..-.=---=---)
  ;; #\A
  (parse-morse 'C.-)
  ;; 42
  (parse-morse 'N....-=..---)
  ;; "FOO BAR"
  (parse-morse "..-.=---=---  -...=.-=.-.")
  ;; (LET ((FOO "BAR"))
  ;;   (PRINT FOO))
  (parse-morse '(.-..=.=- ((..-.=---=--- "-...=.-=.-."))
                 (.--.=.-.=..=-.=- ..-.=---=---))))

;;; The separator may be any character.
(heading "parse-morse, other separators")
(print-each
  ;; FOO
  (parse-morse '..-._---_---)
  (parse-morse '|..-. --- ---|)
  ;; "FOO BAR"
  (parse-morse "..-. --- ---  -... .- .-.")
  (parse-morse "..-./---%---xy-..._.-+.-."))

;;; Executing morse code.
;;; Prints "FOO", then "BAR".
(heading "morse-code")
(morse-code
  (.--.=.-.=..=-.=- "..-.=---=---")
  (.-..=.=- ((..-.=---=--- "-...=.-=.-."))
            (.--.=.-.=..=-.=- ..-.=---=---)))

;;; The other way around
(heading "to-morse")
(print-each
  (to-morse 'foo)
  (to-morse #\a)
  (to-morse 42)
  (to-morse "foo bar")
  (to-morse '(let ((foo "bar"))
              (print foo))))

;;; Convenient when transforming code.
;;; Prints the code executed above.
(heading "morsify")
(print-2-values
 (morsify
   (print "foo")
   (let ((foo "bar"))
     (print foo))))

;;; Other separators can be used.
(heading "morsify-with")
(print-2-values
 (morsify-with (#\_)
   (print "foo bar")
   (print "a b")))

;;; Even for the word separator.
(heading "morsify-with, word separator")
(print-2-values
 (morsify-with (#\_ "///")
   (print "foo bar")
   (print "a b")))

;;; For everyday use.
(heading "string->morse")
(print-each
  ;;; "..-. --- ---  -... .- .-."
  (string->morse "foo bar")
  ;;; "..-. --- ---    -... .- .-."
  (string->morse "foo bar" 4))
  ;;; "FOO BAR"
(heading "morse->string")
(print-each
  (morse->string "..-. --- ---  -... .- .-.")
  (morse->string "..-.=---=---___-...=.-=.-."))

;;; And now for the cool part: Morse mode!
;;; By default, m and M are set as macro characters.
(heading "enable-morse-mode, default")
(enable-morse-mode)

m(.--.=.-.=..=-.=- "..-.=---=---")
M(.-..=.=- ((..-.=---=--- "-...=.-=.-."))
            (.--.=.-.=..=-.=- ..-.=---=---))

;;; Even better
MORSE
(.--.=.-.=..=-.=- "..-.=---=---")
(.-..=.=- ((..-.=---=--- "-...=.-=.-."))
          (.--.=.-.=..=-.=- ..-.=---=---))
...-.- ; End symbol

;;; (disable-morse-mode) contains an m and cannot be called directly.
m(-.._.._..._.-_-..._.-.._._-....-_--_---_.-._..._._-....-_--_---_-.._.)

;;; Less radical: Specify a morse-word and, optionally, an end symbol
(heading "enable-morse-mode, custom")
(enable-morse-mode "#tada" :end)

#t(.--.=.-.=..=-.=- "..-.=---=---")

#tada
(.--.=.-.=..=-.=- "..-.=---=---")
(.-..=.=- ((..-.=---=--- "-...=.-=.-."))
          (.--.=.-.=..=-.=- ..-.=---=---))
:end

(disable-morse-mode)

(enable-morse-mode)
morse
(....=.=.-=-..=..=-.=--. "-=....=.  .=-.=-..")
