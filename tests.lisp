(cz.zellerin.doc:defpackage #:web-tools-test
  (:use #:cl #:wt #:lisp-unit #:lol #:cl-html-parse))

(in-package web-tools-test)

(named-readtables:in-readtable lol-syntax)

(defparameter *test-form*
  (with-output-to-string (o)
    (cl-who:with-html-output (o)
      (:html
       (:body
	(:h1 "Test form")
	(:form :action "doit" :id "a-form"
	       (:input :name "i1" :type :hidden :value "42")
	       (:input :name "i2")))))))

(lisp-unit:define-test forms
  (let ((form (map-tree-if (tag-matcher :form) (parse-html *test-form*))))
    (lisp-unit:assert-true (eq :form (caaar form)))
    (lisp-unit:assert-true (equalp '("i2") (collect-unhidden-parameters form)))
    (lisp-unit:assert-true (equalp '(("i1" . "42")) (collect-hidden-parameters form)))))

(define-rest-base cloudflare-base ("https://api.cloudflare.com/client/v4"
				   (bearer-header-mixin json-checked-content-mixin))
		  (cloudflare-zones "zones"
				    (cloudflare-zone zone-id
						     (cloudflare-zone-records "dns_records"))))

(setq *default-zone-id* (get-authinfo "api.cloudflare.com" "zoneid"))

(lisp-unit:define-test cloudflare
  (lisp-unit:assert-true (cloudflare-zone-records))
  (lisp-unit:assert-error 'json-api-error (cloudflare-zones)))

(lisp-unit:define-test normalize-symbols
    (lisp-unit:assert-true
     (equalp (mapcar #'symbol-to-camelcase
		     '(foo -foo foo-bar foo--bar))
	     '("foo" "Foo" "fooBar" "foo-Bar"))))
