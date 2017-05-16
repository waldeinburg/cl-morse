(in-package :cl-user)
(defpackage #:dk.waldeinburg.cl-morse.asd
  (:use :cl :asdf))

(in-package #:dk.waldeinburg.cl-morse.asd)

(defsystem cl-morse
  :version "0.1"
  :description "Morse support"
  :author "Daniel Lundsgaard Skovenborg <waldeinburg@gmail.com>"
  :licence "BSD"
  :components ((:file "packages")
               (:file "cl-morse")))
