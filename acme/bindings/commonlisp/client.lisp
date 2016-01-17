(defpackage acme.client
  (:use :cl)
  (:export :acme-get
           :acme-set
           :inspect-config
           :list-configs
           :list-schemas
           :inspect-schema
           :connect-acme
           :with-acme-connection
           :reload-configs
           :get-config
           :set-config
           :config-get
           :config-set))

;; Acme socket interface

(in-package :acme.client)

(defparameter *acme-server-host* "localhost")
(defparameter *acme-server-port* 2020)
(defparameter *acme-connection* nil)

;; (defun connect (&key (host *acme-server-host*)
;;                   (port *acme-server-port*))
;;   (setf *acme-connection* (usocket:socket-stream (usocket:socket-connect host port
;;                                                                     :element-type '(unsigned-byte 8)))))

;; (defun acme-get (path)
;;   (write (babel:string-to-octets (format nil "GET ~A~%" path))
;;          :stream *acme-connection*)
;;   (force-output *acme-connection*)
;;   (let ((msg-size (parse-integer (read-line (usocket:socket-stream *acme-connection*)))))
;;     (print msg-size)
;;     (let ((data (make-array msg-size
;;                             :element-type '(unsigned-byte 8))))
;;       (read-sequence data *acme-connection* :end msg-size)
;;       (messagepack:decode data))))

(defun connect-acme (&key (host *acme-server-host*)
                       (port *acme-server-port*))
  (setf *acme-connection* (usocket:socket-stream (usocket:socket-connect host port))))

(defun call-with-acme-connection (function &key (host *acme-server-host*)
                                        (port *acme-server-port*))
  (let ((*acme-connection* (usocket:socket-stream (usocket:socket-connect host port))))
    (funcall function)))

(defmacro with-acme-connection ((&key (host '*acme-server-host*)
                                      (port '*acme-server-port*))
                                &body body)
  `(call-with-acme-connection (lambda () ,@body) :host ,host :port ,port))

(defun send-message (msg)
  (write-line msg *acme-connection*)
  (force-output *acme-connection*)
  (let ((msg-size (parse-integer (read-line *acme-connection*))))
    (let ((data (make-array msg-size
                            :element-type 'character)))
      (let ((result (json:decode-json *acme-connection*)))
        result))))

(defun acme-get (path)
  (send-message (format nil "GET ~A" path)))

(defun list-configs ()
  (send-message "LIST-CONFIGS"))

(defparameter *config* nil "Current config")

(defun set-config (config)
  (assert (member config (list-configs) :test #'string=) nil
          "Config not found: ~A" config)
  (setf *config* config))

(defun get-config ()
  *config*)

(defun config-get (path &optional (config *config*))
  (acme-get (format nil "~A.~A" config path)))

(defun reload-configs ()
  (send-message "RELOAD-CONFIGS"))
