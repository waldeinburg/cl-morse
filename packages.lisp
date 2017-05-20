(in-package :cl-user)

(defpackage #:dk.waldeinburg.cl-morse
  (:nicknames #:cl-morse)
  (:use :cl)
  (:export #:parse-morse
           #:morse-code
           #:enable-morse-mode
           #:...-.-
           #:disable-morse-mode
           #:to-morse
           #:morsify-with
           #:morsify
           #:string->morse
           #:morse->string))
