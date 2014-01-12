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

(defun parse-configuration-option (alist)
  (parse-configuration-option%
   (intern
    (string-upcase (cdr (assoc :type alist)))
    :keyword)
   (cdr (assoc :value alist))))

(defgeneric parse-configuration-option% (option-type option-value))

(defmethod parse-configuration-option% (option-type option-value)
  ;(error "Can't parse option with type ~A" option-type)
  value)

(defmethod parse-configuration-option% ((option-type (eql :string)) value)
  (assert (stringp value))
  value)

(defmethod parse-configuration-option% ((option-type (eql :number)) value)
  (assert (numberp value))
  value)

(defmethod parse-configuration-option% ((option-type (eql :number)) value)
  (assert (typep value 'boolean))
  value)

(defmethod parse-configuration-option% ((option-type (eql :list)) value)
  (assert (typep value 'list))
  value)

(defmethod parse-configuration-option% ((option-type (eql :choice)) value)
  value)

(defmethod parse-configuration-option% ((option-type (eql :filename)) value)
  (pathname value))

(defmethod parse-configuration-option% ((option-type (eql :directory)) value)
  (pathname value))

(defmethod parse-configuration-option% ((option-type (eql :uri)) value)
  (puri:parse-uri value))

(defun configurator-get* (path)
  (parse-configuration-option (configurator-get path)))

(defparameter *configuration* nil)

(defun call-with-configuration (config function)
  (let ((*configuration* config))
    (funcall function)))

(defmacro with-configuration (config &body body)
  `(call-with-configuration ,config (lambda () ,@body)))

(defun get* (path &optional (config *configuration*))
  (configurator-get* (format nil "~A.~A" config path)))

(defun set* (path value &optional (config *configuration*))
  (configurator-set (format nil "~A.~A" config path)
		    value))
