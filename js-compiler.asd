(in-package :common-lisp-user)

(defpackage h2s04.js-compiler-asd-system
  (:nicknames :js-c-asd-sys)
  (:use :common-lisp
	:common-lisp-user
	:asdf))

(in-package :js-c-asd-sys)

(defsystem "h2s04.js-compiler"
  :description "Experimental project to determine how easily js can be compiled to cl"
  :version "18.04.2012"
  :author ".::[h2s04]::KIsLotnIK::."
  :licence: "MIT"
  :depends-on (:parse-js)
  :components ((:file "compiler")))
