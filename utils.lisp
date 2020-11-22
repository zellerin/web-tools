(in-package web-tools)

(define-section @small-utils
  "Random convenience utils"
  (assocd)
  (map-tree-if)
  (get-authinfo)
  (symbol-to-camelcase)
  (normalize-symbol)
  (clist-to-llist))

(declaim (inline assocd))

(defun assocd (item alist &rest pars)
  "Convenience shortcut for (cdr (assoc ...)).
FIXME: this must be in some standard library, but I can't find it."
  (cdr (apply #'assoc item alist pars)))

(defun clist-to-llist (object)
  "Convert list of conses (e.g., used by hunchentoot) to list of lists
  (e.g., used for display in org mode)."
  (mapcar (lambda (a) (list (car a) (cdr a))) object))

;;;; Converting symbols to strings
(defun normalize-symbol (out symbol &optional colon at-sign prefix &rest args)
  "Print to OUT stream SYMBOL converted to camelcase notation.

- The letter after dash is capitalized.
- Two dashes make dash and next letter capitalized.
- First letter is capital if COLON is set."
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

(defun symbol-to-camelcase (symbol &rest args)
  "Generate camelcase name string from symbol. See normalize symbol for
  details."
  (with-output-to-string (s)
    (apply #'normalize-symbol s symbol args)))

(defun map-tree-if (condition tree &optional (result #'list))
  "Traverse a tree and test each node for condition. Append output of
calling `RESULT' on matching nodes.

Result must provide a fresh list that may be possibly destroyed by mapcan."
  (cond
    ((funcall condition tree)
     (funcall result tree))
    ((consp tree)
     (mapcan (lambda (a) (map-tree-if condition a result)) tree))))

;;;; Authinfo
(defvar *authinfo-cache* (make-hash-table :test 'equalp))

(defun get-authinfo (to-match username)
  "Get secret for machine TO-MATCH.

Currently, backend is a cache and the .authinfo file(s)."
  (or (gethash (cons to-match username) *authinfo-cache*)
      (with-open-file (in "~/.authinfo")
	(loop for line = (read-line in nil)
	      while line
	      for system = (nth-value 1 (cl-ppcre:scan-to-strings "machine ([^ ]*) .*login ([^ ]*) .*password ([^ ]*)" line))
	      when (and system (equal (aref system 0) to-match)
			(equal (aref system 1) username))
		do (return (setf (gethash (cons to-match username) *authinfo-cache*)
				 (aref system 2)))))
      (error "Authinfo missing for ~a/~a" to-match username)))


(define-section @http-requests
  "HTTP request helpers.

`HTTP-REQUEST*' is a wrapper over Drakma function with some memory
over cookies and returned content and some convenience features.

`GET-RELOC' finds where a URL relocates (once)."
  (http-request*)
;  (*last-page-data* variable)
  (*last-page-base* variable)
  (*last-page* variable)
  (web-object type)
  (last-html)
  (with-cookie-jar)

  ;;; HTTP header functions
  (get-response-header)
  (get-reloc)
  (get-authorization)

  ;;; URL processing
  (get-parameter-value)
  (build-url))

(defvar *cookie-jar* (make-instance 'cookie-jar)
  "Cookie jar maintained between `HTTP-REQUEST*' calls.")

(defmacro with-cookie-jar (&body body)
  "Run `BODY' with a fresh cookie jar."
  `(let ((*cookie-jar* (make-instance 'cookie-jar)))
     ,@body))

(defclass web-object ()
  ((url          :accessor get-url          :initarg :url
		 :documentation "Original request URL")
   (raw          :accessor get-raw          :initarg :raw)
   (parsed       :accessor get-parsed       :initarg :parsed)
   (final-url    :accessor get-final-url    :initarg :final-url)
   (content-type :accessor get-content-type :initarg :content-type)
   (type         :accessor get-type         :initarg :type))
  (:documentation "Description of result of the last request."))

(defvar *last-page* nil
  "Last visited page as a WEB-OBJECT instance.")

(defvar *last-page-data* nil
  "Cons of content type of the last page requested by `HTTP-REQUEST*'
  and its body. If the CAR is a symbol, the body is parsed (eg. json, html).")

(defvar *last-page-base* nil
  "URL of last page visited by `HTTP-REQUEST*' as an puri object")

(defun last-html ()
  "Parsed html of last requested page. Raises an error if last page
was not an html one."
  (assert (eq 'html (car *last-page-data*)) () "Last result was not html (~s)"
	  (car *last-page-data*))
  (cdr *last-page-data*))

(named-readtables:in-readtable lol-syntax)

(defun http-request* (url &rest args)
  "REPL friendly wrapper of drakma:http-request that
- reuses cookie jar
- parses response body based on the content type if json or html and
  saves it as `*LAST-PAGE-DATA*'
- saves final url to `*LAST-PAGE-BASE*'"

  (let* ((res (multiple-value-call 'list
		(apply 'http-request url :cookie-jar *cookie-jar* args)))
	 (content-type (assocd :content-type (nth 2 res))))
    (setq *last-page*
	  (make-instance 'web-object
			 :url url
			 :final-url (nth 3 res)
			 :raw (car res)
			 :type
			 (cond
			   ((#~m&^text/html& content-type)
			    'html)
			   (t 'nil))
			 :parsed
			 (cond
			   ((#~m&^text/html& content-type)
			    (parse-html (car res)))
			   (t nil))
			 :content-type content-type))
    (setq *last-page-data*
	  (cond
	    ((#~m&^text/html& content-type)
	     (cons 'html (parse-html (car res))))
	    (t (cons content-type (car res))))
	  *last-page-base* (nth 3 res))
    (cons (cdr *last-page-data*) (cdr res))))

(defun get-response-header (header url pars)
  "Run an http request and get a header from http response."
  (assocd header (nth-value 2 (apply 'http-request url :force-binary t pars))))

(defun get-authorization (url &rest pars)
  "Run an http request and get a Authorization header from http response."
  (get-response-header :authorization url pars))

(defun get-reloc (url &rest pars)
  "Run an http request and get a Location header from http response."
  (get-response-header :location url (list* :redirect nil pars)))

(defun get-parameter-value (par url) ;; export
  "Get value of query parameter in `URL'."
  (let* ((start (+ 1 (length par) (search par url)))
	 (end (position #\& url :start (1+ start))))
    (do-urlencode:urldecode (subseq url start end))))

(defun build-url (&rest components)
  "Build URL from whatever parts with added slashes between the parts.
No fancy uri-like interpolation."
  (format nil "~{~a~^/~}" components))

;;;; html handling
(define-section @html-parsing
  "Functions to extract text and forms from a HTML.


Conventions:
- Input of the functions is typically parsed HTML. If full HTML
  document is expected, (last-html) is default input."
	  (clean-html)
	  (collect-forms) (collect-hidden-parameters) (collect-unhidden-parameters)
	  (html-post-form)
	  (tag-matcher))

(defun clean-html (&optional (html (last-html)))
  "Plain text from the html.

Note that uncommented CSS is included (maybe a bug)"
  (labels ((ca?r (a)
	     (if (consp (car a)) (caar a) (car a)))
	   (clean-tree (tree)
		(cond ((stringp tree) tree)
		      ((eq tree :br) "
")
		      ((atom tree) "")
		      ((and (consp tree)
			    (or
			     (member (car tree) '(:script :head :comment :style))
			     (and (consp (car tree))
				  (member (caar tree) '(:script :head :style)))))
		       nil)
		      (t (apply 'concatenate 'string
				(case (ca?r tree)
				  ((:p :div) (string #\NewLine))
				  ((:img :th :td :li) " ")
				  (t ""))
				(mapcar #'clean-tree (cdr tree)))))))
    (cl-ppcre:regex-replace-all (format nil "~%{3,}")
				(nth-value 0 (decode-entities (clean-tree html)))
				(string #\NewLine))))

;;;; Forms handling
(defun html-tag-attr (element attribute &optional default)
  "Value of `ATTRIBUTE' in a parsed html `ELEMENT', or `DEFAULT' "
  (declare ((or null string) default)
	   (keyword attribute))
  (if (consp (car element)) (decode-entities (getf (cdar element) attribute default))
      default))

(defun tag-matcher (tag &rest pars-and-vals)
  "Return a function that checks whether a TREE represents a html
`TAG' with required attributes and values.

Designed to be passed to `MAP-TREE-IF' as its CONDITION parameter."
  (lambda (tree)
    (and (consp tree)
	 (or
	  (and (null pars-and-vals) (eq (car tree) tag))
	  (and (consp (car tree))
	       (eq (caar tree) tag)))
	 (loop for (attr val) on pars-and-vals by #'cddr
	       unless (string-equal val (cadr (member attr (cdar tree))))
		 do (return nil)
	       finally (return t)))))

(defun collect-forms (&optional (parsed-html (last-html)))
  "Collect all forms in the `PARSED-HTML'"
  (map-tree-if (tag-matcher :form) parsed-html))

(defun collect-hidden-parameters (&optional (form (car (collect-forms))))
  "Return name and value of all hidden parameters of parsed FORM, by
  default first form of last HTML page."
  (map-tree-if (tag-matcher :input :type "hidden") form
	       (lambda (a)
		 (list (cons (getf (cdar a) :name)
			     (decode-entities (getf (cdar a) :value)))))))

(defun collect-unhidden-parameters (&optional (form (car (collect-forms))))
  "Collect visible (not hidden) parameters of parsed form, by default
first form of last HTML page loaded."
  (map-tree-if (tag-matcher :input) form
	       (lambda (form)
		 (unless (equalp (html-tag-attr form :type) "hidden")
		   (list (html-tag-attr form :name))))))

(defun html-post-form (&key (base *last-page-base*) (form (car (collect-forms))) pars)
  "Post a FORM with new parameters.

The FORM is by default first form of last HTML page.

BASE is used as base if the :action in the form is relative.

PARS is a list of (key . value) conses to be used as
parameters. Hidden parameters are included implicitly."
  (http-request* (puri:merge-uris (html-tag-attr form :action ".") base)
		 :method (find-symbol (string-upcase
				       (html-tag-attr form :method "GET")) :keyword)
	:parameters (append pars (collect-hidden-parameters form))))
