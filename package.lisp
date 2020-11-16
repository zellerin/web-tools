(cz.zellerin.doc:defpackage #:cz.zellerin.web-tools
  (:nicknames #:wt #:web-tools)
  (:local-nicknames (#:doc #:cz.zellerin.doc))
  (:use #:cl #:lol #:drakma #:cl-json #:cl-html-parse #:html-entities)
  (:import-from  #:cz.zellerin.doc #:define-section)
  (:shadow drakma:get-content-type)
  (:documentation
   "Set of tools used for accessing web pages and data.")
  (:sections
   @small-utils @http-requests @html-parsing @rest-client)

  )
