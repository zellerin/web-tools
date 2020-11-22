(asdf:defsystem web-tools
  :version "0"
  :description "Collected web tools used for almost anything."
  :maintainer "Tomas Zellerin <tomas@zellerin.cz>"
  :author "Tomas Zellerin <tomas@zellerin.cz>"
  :licence "Private"
  :depends-on (drakma cl-html-parse let-over-lambda cl-json html-entities
		      cz.zellerin.doc do-urlencode closer-mop)
  :serial t
  ;; components likely need manual reordering
  :components
  ((:file "package")
   (:static-file "web-tools.asd" :pathname "web-tools.asd")
   (:file "utils")
   (:file "json")
   (:file "rest-client"))
  ;; :long-description ""
  )

(asdf:defsystem web-tools/test
  :version "0"
  :description "Collected web tools used for almost anything."
  :maintainer "Tomas Zellerin <tomas@zellerin.cz>"
  :author "Tomas Zellerin <tomas@zellerin.cz>"
  :licence "Private"
  :depends-on (web-tools lisp-unit cl-who)
  :serial t
  ;; components likely need manual reordering
  :components
  ((:file "tests"))
  ;; :long-description ""
)
