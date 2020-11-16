(in-package web-tools)

(shadow 'drakma:get-content-type)

(define-section @rest-client
  "Generate functions that represent REST api calls.

Supports parameters as HTTP parameters as well as part of URL.

Represents individual API endpoints as CLOS classes, so that

additional features (error detection and raising, json, ...) can be
added relatively easily to calls.

"
  (define-rest-base)
  (define-endpoint)
  (rest-query)
  (simple-rest-endpoint type)
  (auth-header-mixin type)
  (bearer-header-mixin type)
  (json-content-mixin type)
  (json-api-error)
  (json-checked-content-mixin type))

(define-method-combination combine-uri () ((primary () :order :most-specific-last
						       :required t))
  `(concatenate 'string
		(call-method ,(car primary))
		,@(mapcan (lambda (method)
			    (list "/" `(call-method ,method)))
			  (cdr primary))))

(defgeneric get-uri (endpoint)
  (:method-combination combine-uri))

(defclass simple-rest-endpoint ()
  ((method             :accessor get-method             :initarg :method
		       :type (member :GET :POST)
		       :documentation "HTTP method (verb) to use for the call.")
   (additional-headers :accessor get-additional-headers :initarg :additional-headers
		       :documentation "HTTP headers added for, e.g., authentication. Note that *COOKIE-JAR* is used by default, so that cookies should not be needed.")
   (BODY               :accessor get-body               :initarg :body)
   (content-type       :accessor get-content-type       :initarg :content-type))
  (:default-initargs :method :GET :body nil
		     :content-type nil
   :additional-headers nil)
  (:documentation
   "API endpoint that can be called by REST-QUERY. Individual slots
   roughly correspond to parameters to DRAKMA:HTTP-REQUEST."))

(defgeneric post-process (type raw)
  (:method (type raw)
    raw))

#+nil (defun decode-content-by-type (headers body)
  (multiple-value-bind (type subtype)
      (drakma::get-content-type headers)
    (when (and type subtype
	       (equal type "application")
	       (equal subtype "json"))
      (let ((json  (cl-json:decode-json-from-string body)))
	(aif (assocd :errors json)
	     (error "API error ~s" (cdr it))
	     json)))))

(defun rest-query (class &rest pars)
  (let ((e (apply 'make-instance class pars)))
    ;; Make a request and parse json.
    (let* ((*text-content-types*
	     `(("application" . "json")
	       ,@*text-content-types*)))
      (multiple-value-bind (body status headers last-uri stream closep reason)
	  (http-request (get-uri e)
			:additional-headers (get-additional-headers e)
			:method (get-method e)
			:parameters (mapcan (lambda (p)
					      (etypecase p
						(cons
						 (list p))
						(symbol
						 (when (slot-boundp e p)
						   (list (cons (string-downcase (symbol-name p))
							       (slot-value e p)))))))
					    (get-parameters e))
			:cookie-jar *cookie-jar*
			:content (get-body e)
			:content-type (when (get-body e) (get-content-type e))
			:accept (get-content-type e))
	(declare (ignore stream closep))
	(unless (stringp body)
	  (multiple-value-call #'warn "Forced conversion of body with content type ~a"
	    (assocd "conversion" headers :test #'string-equal))
	  (setq body (map 'string 'code-char body)))

	(values
	 (setq *last-page-data*
	       (post-process e body))
	 status headers last-uri reason)))))


#+nil ((defclass json-post-endpoint (simple-rest-endpoint)
	  ()
	  (:default-initargs
	   :method :POST
	   :content-type "application/json"))

       (defmethod get-body :around ((e json-post-endpoint))
	 (cl-json:encode-json-to-string (call-next-method))))

(defgeneric get-parameters (e)
  (:method-combination append))


(defmacro define-rest-base (name (url &optional headers-or-class) &rest tree)
  "Define group of REST endpoints starting with same `URL' prefix.
NAME is used internally for naming a class of type `SIMPLE-REST-ENDPOINT' with parameters initialized from `ADDITIONAL-HEADERS'."
  (let ((additional-headers (remove-if-not #'consp headers-or-class))
	(mixins (remove-if-not #'symbolp headers-or-class)))
    `(progn
       (defclass ,name (,@mixins simple-rest-endpoint)
	 ()
	 ,@(when additional-headers
	     `((:default-initargs
		:additional-headers ',additional-headers))))

       (defmethod get-uri ((e ,name)) ,url)
       ,@(mapcar (lambda (item) `(define-endpoint ,(car item) ,name ,@ (cdr item))) tree))))

;;;; Converting symbols to strings
(defun normalize-symbol (out symbol &optional colon at-sign prefix &rest args)
  (declare (ignore args at-sign))
  (with-input-from-string (in (symbol-name symbol))
    (when prefix (write-char prefix out))
    (loop with capital = colon
	  for c =(read-char in nil nil)
	  while c
	  do
	     (case c
	       ((#\-) ;; one dash makes next char capital
		(if capital ;; two dashes make a dash and next capital
		    (write-char c out)
		    (setf capital t)))
	       ((#\.)
		(write-char c out)
		(setf capital t))
	       (t (write-char (if capital (char-upcase c)
				  (char-downcase c))
			      out)
		(setf capital nil))))))

(defmacro define-endpoint (name base &optional (segment (symbol-to-camelcase name)) &rest tree)
  "Define a callable REST API endpoint represented by a class named `NAME' with `BASE' as a single superclass.
SEGMENT is either
- a string that will be added to url of base class; it may contain slashes inside, or
- a symbol that will represent a parameter spliced to the base url
The remaining arguments may be either
- symbols (represent variables for HTTP parameters) or
- conses that are interpreted as children endpoint."
  (check-type name symbol)
  (check-type base symbol)
  (check-type segment (or symbol string))
  (let ((subs (remove-if-not 'consp tree))
	(pars (remove-if-not 'symbolp tree)))
    `(progn
       ,(if (symbolp segment)
	    `(define-item-endpoint ,name ,base ,segment)
	    `(progn
	       (defclass ,name (,base)
		 ,(mapcar (lambda (par) (list par :initarg (intern (symbol-name par) :keyword) :reader (symb "GET-" par))) pars))
	       (defmethod get-parameters append ((e ,name)) (declare (ignore e)) ',pars)
	       (defmethod get-uri ((e ,name)) ,segment)))
       ,@(mapcar (lambda (item) `(define-endpoint ,(car item) ,name ,@ (cdr item))) subs)
       (defun ,name (&rest pars) (apply #'rest-query ',name pars)))))

(defmacro define-item-endpoint (name base var-name)
  "Define endpoint that represents a name in variable"
  (let ((keyword-name (intern (symbol-name var-name) 'keyword))
	(default-name  (symb "*DEFAULT-" var-name "*")))
    `(progn
       (defvar ,default-name)
       (defclass ,name (,base)
	 ((,var-name :accessor ,(symb "GET-" var-name) :initarg ,keyword-name))
	 (:default-initargs ,keyword-name ,default-name))

       (defmethod get-uri ((e ,name)) (,(symb "GET-" var-name) e)))))

(defclass auth-header-mixin ()
  ()
  (:documentation "Use this mixin to provide Authorization: <secret> header.
Secret is taken from ~/.authinfo (and cached)."))

(defmethod get-additional-headers :around ((o auth-header-mixin))
  (acons "Authorization" (get-authinfo (puri:uri-host (puri:parse-uri (wt::get-uri o))) "api")
		 (call-next-method)))

(defclass bearer-header-mixin ()
  ()
  (:documentation "Use this mixin to provide Authorization: Bearer <secret> header.
Secret is taken from ~/.authinfo (and cached)."))

(defmethod get-additional-headers :around ((o bearer-header-mixin))
  (acons "Authorization"
	 (concatenate 'string "Bearer "
		      (get-authinfo (puri:uri-host (puri:parse-uri (wt::get-uri o))) "api"))
		 (call-next-method)))

(defclass json-content-mixin ()
  ()
  (:default-initargs :content-type  "application/json")
  (:documentation "Use this mixin to
- Ensure request and expected body is Application/json
- If body is present, it is encoded sexp -> json"))

(defmethod get-body :around ((o json-content-mixin))
  (let ((as-json (call-next-method)))
    (when as-json
      (cl-json:encode-json-to-string as-json))))

(defmethod post-process ((o json-content-mixin) body)
  (let ((json  (cl-json:decode-json-from-string body)))
    json))

(define-condition json-api-error (simple-error)
  ((code    :accessor get-code    :initarg :code)
   (message :accessor get-message :initarg :message))
  (:documentation "Condition signalled when REST server sends back
  error indication that is detected by some mixins (e.g.,
  JSON-CHECKED-CONTENT-MIXIN). "))

(defmethod print-object ((o json-api-error) s)
  (print-unreadable-object (o s :type t :identity nil)
    (format s "(~d) ~a" (get-code o) (get-message o))))

(defclass json-checked-content-mixin (json-content-mixin)
  ()
  (:documentation
   "Wrapper for json API that checks that there is no error tag
  present. If it is, JSON-API-ERROR is signalled with provided message
  and code.

  This is modeled after cloudflare format, so it also checks that the
  succes tag is present and that the data are wrapped in RESULTS
  tag. Failure to do this causes warnings."))

(defmethod post-process :around ((o json-checked-content-mixin) body)
  (let ((json  (call-next-method)))
    (aif (assocd :errors json)
	 (destructuring-bind (code message) (extract-tags it '(:code :message))
	   (error 'json-api-error :code code :message message)))
    (unless (eq t (assocd :success json))
      (warn "Success value not set"))
    (or (assocd :result json)
	(warn "Result not set, maybe you use wrong class?")
	json)))

(defclass templated-body-mixin ()
  ((template   :accessor get-template   :initarg :template)
   (template-parameters :accessor get-template-parameters :initarg :pars))
  (:default-initargs :template nil :pars nil))

(defmethod get-body ((o templated-body-mixin))
  (fill-template (get-template o) (get-template-parameters o)))
