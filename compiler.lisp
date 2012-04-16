;; (defparameter *parsed-js* (second
;; 			   (with-open-file (js-file "~/code/reactor/js-benchmark/sample.js")
;; 			     (parse-js:parse-js js-file))))

;; (eval `(progn ,@(traverse-ast *parsed-js* nil)))


;; Engine
(defparameter *tokens-table* (make-hash-table))

(defun process-token (token)
  (let ((token-type (first token)))
    (apply (gethash token-type *tokens-table* (lambda (&rest args)
						(declare (ignore args))
						``(unknown token ,,token-type)))
	   (rest token))))

(defun traverse-ast (list acc)
  (cond ((null list) (nreverse acc))
	(t (traverse-ast (rest list)
			 (cons (process-token (first list))
			       acc)))))

;; Tokens


(defun var-token (var-definition)
  `(setf ,(intern (first (first var-definition)))
	 ,(third  (first var-definition))))

(defparameter *current-block* nil)

(defun defun-token (name params-list body)
  (let ((*current-block* (intern name)))
    `(defun ,(intern name) ,(mapcar (lambda (x)
				      (intern x))
				    params-list)
       ,@(traverse-ast body nil))))

(defun return-token (body)
  `(return-from ,*current-block*
     ,@(traverse-ast (list body) nil)))

(defun binary-token (operator param1 param2)
  `(funcall ',(intern (symbol-name operator))
	    ,@(traverse-ast (list param1) nil)
	    ,@(traverse-ast (list param2) nil)))

(defun call-token (fun-name fun-params)
  `(apply ',@(traverse-ast (list fun-name) nil)
	  (list ,@(traverse-ast fun-params nil))))

(defun name-token (name)
  (intern name))

(setf (gethash :var    *tokens-table*) #'var-token)
(setf (gethash :defun  *tokens-table*) #'defun-token)
(setf (gethash :return *tokens-table*) #'return-token)
(setf (gethash :binary *tokens-table*) #'binary-token)
(setf (gethash :call   *tokens-table*) #'call-token)
(setf (gethash :name   *tokens-table*) #'name-token)