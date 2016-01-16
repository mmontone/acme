(in-package :acme)

;; Acme API via command line

(defparameter *acme*
  ;(asdf:system-relative-pathname :acme "../../acme")
  "acme")
(defparameter *configuration-schemas-pathname* nil)
(defparameter *configurations-pathname* nil)
(defparameter *configurations* nil)

(defclass configuration ()
  ((name :initarg :name
	 :initform (error "Provide the name")
	 :accessor name)
   (options :accessor options
	    :initform (make-hash-table :test #'equalp))))

(defmethod print-object ((config configuration) stream)
  (print-unreadable-object (config stream :type t :identity t)
    (format stream "~S" (name config))))

(defun acme-setup (schemas-pathname configs-pathname)
  (setf *configuration-schemas-pathname* schemas-pathname)
  (setf *configurations-pathname* configs-pathname)
  (load-configurations)
  t)

(defun acme-setup-example ()
  (acme-setup
   (asdf:system-relative-pathname :acme "../../doc/example/acme.schema")
   (asdf:system-relative-pathname :acme "../../doc/example/acme.config")))

(defun load-configurations ()
  (setf *configurations* (make-hash-table :test #'equalp))
  (loop for config-name in (acme-list-configs)
       do
       (let ((data (acme-inspect config-name)))
	 (let ((config (make-instance 'configuration
				      :name config-name)))
	   (loop for option-data in data
	      do (setf (gethash (cdr (assoc :option option-data))
				(options config))
		       (parse-configuration-option option-data)))
	   (setf (gethash config-name *configurations*)
		 config)))))

(defun acme-inspect (config)
  (let ((command (format nil "~A --schemas ~A --configs ~A -i ~A --json"
			 *acme*
			 *configuration-schemas-pathname*
			 *configurations-pathname*
			 config)))
    (multiple-value-bind (output error status) (trivial-shell:shell-command command)
      (if (not (equalp status 0))
	  (error error))
      (json:decode-json-from-string output))))

(defun acme-get (path)
  (let ((command (format nil "~A --schemas ~A --configs ~A --get '~A' --json"
			 *acme*
			 *configuration-schemas-pathname*
			 *configurations-pathname*
			 path)))
    (multiple-value-bind (output error status) (trivial-shell:shell-command command)
      (if (not (equalp status 0))
	  (error error))
      (json:decode-json-from-string output))))

(defun acme-set (option value)
  (let ((command (format nil "~A --schemas ~A --configs ~A --set '~A' --json"
			 *acme*
			 *configuration-schemas-pathname*
			 *configurations-pathname*
			 (json:encode-json-to-string (list option value)))))
    (multiple-value-bind (output error status) (trivial-shell:shell-command command)
      (declare (ignore output))
      (if (not (equalp status 0))
	  (error error))
      t)))

(defun acme-list-configs ()
  (let ((command (format nil "~A --schemas ~A --configs ~A --list --json"
			 *acme*
			 *configuration-schemas-pathname*
			 *configurations-pathname*)))
    (multiple-value-bind (output error status) (trivial-shell:shell-command command)
      (if (not (equalp status 0))
	  (error error))
      (json:decode-json-from-string output))))

(defun acme-get* (path)
  (parse-configuration-option (acme-get path)))

(defparameter *configuration* nil)

(defun call-with-configuration (config function)
  (let ((*configuration* (if (stringp config)
			     (get-config config)
			     config)))
    (funcall function)))

(defmacro with-configuration (config &body body)
  `(call-with-configuration ,config (lambda () ,@body)))

(defun config-get (path &optional (config *configuration*))
  (gethash path (options config)))

(defun config-set (path value &optional (config *configuration*))
  (setf (gethash path (options config)) value))

(defun in-configuration (config)
  (setf *configuration* (if (stringp config)
			    (get-config config)
			    config)))

(defun get-config (name)
  (or
   (gethash name *configurations*)
   (error "~A configuration not loaded" name)))

(defun get-current-configuration ()
  *configuration*)
