(defpackage configurator
  (:nicknames :config)
  (:use :cl)
  (:export #:configurator-get
	   #:configurator-set
	   #:configurator-setup
	   #:configurator-setup-example
	   #:configurator-list-configs
	   #:configurator-inspect))
