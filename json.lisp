(in-package web-tools)

(define-section @js-tools
  "Function on lists that facilitate work with json data after parsing
  them with cl-json."
  (extract-tags) (extract-tags-from-list)
  (fill-template)
  (fill-tempate*))

(defun extract-tags (form tags)
  "Extract cdrs of forms with car in tags.

Not optimized for speed:
- iterates tags twice
- conses a lot
- etc"
  (labels
      ((rec (form)
	 (cond
	   ((not (consp form)) nil)
	   ((member (car form) tags)
	    (list form))
	   (t (append
	       (rec (car form))
	       (rec (cdr form)))))))
    (let ((found (rec form)))
      (mapcar (lambda (a) (assocd a found)) tags))))

(defun extract-tags-from-list (tags data)
     (mapcar (alexandria:rcurry #'extract-tags tags)  data))

(defun fill-template (template keys-values)
  "Fill a template suitable for json with values "
  (if (consp template)
      (cond ((and (keywordp (car template))
		  (getf keys-values (car template)))
	     (setf (cdr template)
		   (getf keys-values (car template)))
	     template)
	  (t (cons (fill-template (car template) keys-values)
		   (fill-template (cdr template) keys-values))))
    template))



(defun fill-template* (template &rest keys-values)
  (fill-template template keys-values))
