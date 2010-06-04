
(in-package :cl-user)

(defpackage :WC.system
  (:use :cl :asdf))

(in-package :WC.system)

(defsystem :WC
  :name "WC"
  :description "WC a voting machine (German: Wahl Computer)"
  :version "0.1"
  :author "Franz Haas franz_haas@lavabit.com"
  :maintainer "Franz Haas franz_haas@lavabit.com"
  :licence "GPL"
  :depends-on ("yason")
  :components ((:file "WC-internals-data-structures")
	       (:file "WC-internals-crypto")
	       (:file "WC-internals-UI")
	       (:file "WC-internals-voting")))


;; Since voting is very sensitive, I object to distribute this
;; functionality without source. Therefore GPL.