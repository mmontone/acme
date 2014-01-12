(in-package :configurator)

;; Configurator API via command line

(defparameter *configurator*
  ;(asdf:system-relative-pathname :configurator "../../configurator")
  "configurator")
(defparameter *configuration-schemas-pathname* nil)
(defparameter *configurations-pathname* nil)

(defun configurator-setup (schemas-pathname configs-pathname)
  (setf *configuration-schemas-pathname* schemas-pathname)
  (setf *configurations-pathname* configs-pathname)
  t)

(defun configurator-setup-example ()
  (configurator-setup
   (asdf:system-relative-pathname :configurator "../../doc/example/configurator.schema")
   (asdf:system-relative-pathname :configurator "../../doc/example/configurator.config")))

(defun configurator-inspect (config)
  (let ((command (format nil "~A --schemas ~A --configs ~A -i ~A --json"
			 *configurator*
			 *configuration-schemas-pathname*
			 *configurations-pathname*
			 config)))
    (multiple-value-bind (output error status) (trivial-shell:shell-command command)
      (if (not (equalp status 0))
	  (error error))
      (cl-json:decode-json-from-string output))))

(defun configurator-get (path)
  (let ((command (format nil "~A --schemas ~A --configs ~A --get '~A' --json"
			 *configurator*
			 *configuration-schemas-pathname*
			 *configurations-pathname*
			 path)))
    (multiple-value-bind (output error status) (trivial-shell:shell-command command)
      (if (not (equalp status 0))
	  (error error))
      (cl-json:decode-json-from-string output))))

(defun configurator-set (option value)
  (let ((command (format nil "~A --schemas ~A --configs ~A --set '~A' --json"
			 *configurator*
			 *configuration-schemas-pathname*
			 *configurations-pathname*
			 (json:encode-json-to-string (list option value)))))
    (multiple-value-bind (output error status) (trivial-shell:shell-command command)
      (declare (ignore output))
      (if (not (equalp status 0))
	  (error error))
      t)))

(defun configurator-list-configs ()
  (let ((command (format nil "~A --schemas ~A --configs ~A --list --json"
			 *configurator*
			 *configuration-schemas-pathname*
			 *configurations-pathname*)))
    (multiple-value-bind (output error status) (trivial-shell:shell-command command)
      (if (not (equalp status 0))
	  (error error))
      (cl-json:decode-json-from-string output))))
