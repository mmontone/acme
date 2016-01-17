(defpackage acme.client
  (:use :cl)
  (:export :acme-get
           :acme-set
           :inspect-config
           :list-configs
           :list-schemas
           :inspect-schema
           :connect
           :with-acme-connection))

;; Acme socket interface

(in-package :acme.client)

(defparameter *acme-server-host* "localhost")
(defparameter *acme-server-port* 2020)
(defparameter *connection* nil)

;; (defun connect (&key (host *acme-server-host*)
;;                   (port *acme-server-port*))
;;   (setf *connection* (usocket:socket-stream (usocket:socket-connect host port
;;                                                                     :element-type '(unsigned-byte 8)))))

;; (defun acme-get (path)
;;   (write (babel:string-to-octets (format nil "GET ~A~%" path))
;;          :stream *connection*)
;;   (force-output *connection*)
;;   (let ((msg-size (parse-integer (read-line (usocket:socket-stream *connection*)))))
;;     (print msg-size)
;;     (let ((data (make-array msg-size
;;                             :element-type '(unsigned-byte 8))))
;;       (read-sequence data *connection* :end msg-size)
;;       (messagepack:decode data))))

(defun connect (&key (host *acme-server-host*)
                  (port *acme-server-port*))
  (setf *connection* (usocket:socket-stream (usocket:socket-connect host port))))

(defun send-message (msg)
  (write-line msg *connection*)
  (force-output *connection*)
  (let ((msg-size (parse-integer (read-line *connection*))))
    (let ((data (make-array msg-size
                            :element-type 'character)))
      (let ((result (json:decode-json *connection*)))
        result))))

(defun acme-get (path)
  (send-message (format nil "GET ~A" path)))

(defun list-configs ()
  (send-message "LIST-CONFIGS"))
