(defpackage acme
  (:nicknames :config)
  (:use :cl)
  (:export #:acme-get
	   #:acme-set
	   #:acme-setup
	   #:acme-setup-example
	   #:acme-list-configs
	   #:acme-inspect
	   #:acme-get*
	   #:parse-configuration-option
	   #:with-configuration
	   #:config-get
	   #:config-set
	   #:in-configuration
	   #:get-current-configuration))
