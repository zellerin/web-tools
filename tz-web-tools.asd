(asdf:defsystem tz-web-tools
  :version "0"
  :description "Collected web tools used for almost anything."
  :maintainer "Tomas Zellerin <tomas@zellerin.cz>"
  :author "Tomas Zellerin <tomas@zellerin.cz>"
  :licence "Private"
  :depends-on (drakma cl-html-parse let-over-lambda cl-json html-entities
		      cz.zellerin.doc do-urlencode #+nil gzip-stream)
  :serial t
  ;; components likely need manual reordering
  :components
  ((:file "package")
   (:static-file "tz-web-tools.asd" :pathname "tz-web-tools.asd")
   (:file "utils")
   (:file "json")
   (:file "rest-client"))
  ;; :long-description ""
  )

(asdf:defsystem tz-web-tools/test
  :version "0"
  :description "Collected web tools used for almost anything."
  :maintainer "Tomas Zellerin <tomas@zellerin.cz>"
  :author "Tomas Zellerin <tomas@zellerin.cz>"
  :licence "Private"
  :depends-on (tz-web-tools lisp-unit cl-who)
  :serial t
  ;; components likely need manual reordering
  :components
  ((:file "tests"))
  ;; :long-description ""
)
