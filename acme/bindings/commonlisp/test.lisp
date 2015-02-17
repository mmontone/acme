(defpackage acme-test
  (:use :cl :config))

(in-package :acme-test)

(defparameter *configs-file* (asdf:system-relative-pathname :acme
					     "../../doc/example/acme.config"))
(load-configs *configs-file*)
