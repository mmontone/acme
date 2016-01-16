(in-package :acme)

(defun parse-configuration-option (alist)
  (parse-configuration-option%
   (intern
    (string-upcase (cdr (assoc :type alist)))
    :keyword)
   (cdr (assoc :value alist))))

(defgeneric parse-configuration-option% (option-type option-value))

(defmethod parse-configuration-option% (option-type value)
  ;(error "Can't parse option with type ~A" option-type)
  value)

(defmethod parse-configuration-option% ((option-type (eql :string)) value)
  (assert (stringp value))
  value)

(defmethod parse-configuration-option% ((option-type (eql :number)) value)
  (assert (numberp value))
  value)

(defmethod parse-configuration-option% ((option-type (eql :boolean)) value)
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
  (pathname (format nil "~A/" value)))

(defmethod parse-configuration-option% ((option-type (eql :uri)) value)
  (puri:parse-uri value))

(defmethod parse-configuration-option% ((option-type (eql :time)) value)
    (local-time:parse-timestring value :start 10))

(defmethod parse-configuration-option% ((option-type (eql :date)) value)
    (local-time:parse-timestring value :end 8
				 :date-separator #\/))

(defmethod parse-configuration-option% ((option-type (eql :datetime)) value)
  (destructuring-bind (date time) value
    (let ((parsed-time (local-time:parse-timestring time :start 10))
	  (parsed-date (local-time:parse-timestring date :end 8
						    :date-separator #\/)))
      (local-time:encode-timestamp 0
				   (local-time:timestamp-second parsed-time)
				   (local-time:timestamp-minute parsed-time)
				   (local-time:timestamp-hour parsed-time)
				   (local-time:timestamp-day parsed-date)
				   (local-time:timestamp-month parsed-date)
				   (local-time:timestamp-year parsed-date)))))

;(defmethod parse-configuration-option% ((option-type (eql :color)) value)
;  (cl-colors:hex-to-rgb (subseq value 1)))
(defun parse-configuration-option (alist)
  (parse-configuration-option%
   (intern
    (string-upcase (cdr (assoc :type alist)))
    :keyword)
   (cdr (assoc :value alist))))

(defgeneric parse-configuration-option% (option-type option-value))

(defmethod parse-configuration-option% (option-type value)
  ;(error "Can't parse option with type ~A" option-type)
  value)

(defmethod parse-configuration-option% ((option-type (eql :string)) value)
  (assert (stringp value))
  value)

(defmethod parse-configuration-option% ((option-type (eql :number)) value)
  (assert (numberp value))
  value)

(defmethod parse-configuration-option% ((option-type (eql :boolean)) value)
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
  (pathname (format nil "~A/" value)))

(defmethod parse-configuration-option% ((option-type (eql :uri)) value)
  (puri:parse-uri value))

(defmethod parse-configuration-option% ((option-type (eql :time)) value)
    (local-time:parse-timestring value :start 10))

(defmethod parse-configuration-option% ((option-type (eql :date)) value)
    (local-time:parse-timestring value :end 8
				 :date-separator #\/))

(defmethod parse-configuration-option% ((option-type (eql :datetime)) value)
  (destructuring-bind (date time) value
    (let ((parsed-time (local-time:parse-timestring time :start 10))
	  (parsed-date (local-time:parse-timestring date :end 8
						    :date-separator #\/)))
      (local-time:encode-timestamp 0
				   (local-time:timestamp-second parsed-time)
				   (local-time:timestamp-minute parsed-time)
				   (local-time:timestamp-hour parsed-time)
				   (local-time:timestamp-day parsed-date)
				   (local-time:timestamp-month parsed-date)
				   (local-time:timestamp-year parsed-date)))))

;(defmethod parse-configuration-option% ((option-type (eql :color)) value)
;  (cl-colors:hex-to-rgb (subseq value 1)))
