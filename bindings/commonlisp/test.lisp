(defpackage configurator-test
  (:use :cl :config))

(in-package :configurator-test)

(defparameter *configs-file* (asdf:system-relative-pathname :configurator
					     "../../doc/example/configurator.config"))
(load-configs *configs-file*)
