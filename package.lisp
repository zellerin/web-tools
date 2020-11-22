(cz.zellerin.doc:defpackage #:cz.zellerin.web-tools
  (:nicknames #:wt #:web-tools)
  (:local-nicknames (#:doc #:cz.zellerin.doc))
  (:use #:cl #:lol #:drakma #:cl-json #:cl-html-parse #:html-entities)
  (:import-from  #:cz.zellerin.doc #:define-section)
  (:shadow drakma:get-content-type)
  (:documentation
   "This is a non-supported library for web requests manipulation. The main parts are:
- simple rest clients implementation that handles endpoints - see example in tests.lisp:
- endpoints definition
- jsoning and unjsoning,
- getting token from authinfo file
- wrapper over http-request so that it can be used in repl - see http-requests* docstring.

Not expected to be useful for anyone else:
- uses custom repl-oriented doc package so cannot be loaded directly
- not cleaned up.
- sbcl specific
- .authinfo is not much popular these days
Set of tools used for accessing web pages and data.
")
  (:sections
   @http-requests @small-utils @http-requests @html-parsing @rest-client)

  )
