;;;-------------------------------------------------------------
;;; File    : company-distel-lib.el
;;; Author  : Sebastian Weddmark Olsson
;;;           github.com/sebastiw
;;; Purpose : Library for company-distel. Can get documentation
;;;           and do Distel request. :) ooooh
;;; 
;;; Created : August 2012 as an internship at Klarna AB
;;; Comment : Please let me know if you find any bugs or you
;;;           want some feature or something
;;;-------------------------------------------------------------
(require 'distel)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; docs funs         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-company-get-docs-from-internet-p (mod fun) ;; maybe version?
  "Download the documentation from internet."
  (let ((str
	 (with-current-buffer 
	     (url-retrieve-synchronously (format "http://www.erlang.org/doc/man/%s.html" mod))
	   (goto-char (point-min))
	   
	   ;; find <p> containing <a name="module"> then
	   ;; find <div class="REFBODY">
	   
	   (let* ((m (re-search-forward (or (and fun (format "<p>.*?<a name=\"%s.*?\">" fun))
					    "<h3>DESCRIPTION</h3>") nil t))
		  (beg (and m (match-end 0)))
		  (end (and m (progn (re-search-forward "</p>.*?</div>" nil t)
				     (match-end 0)))))
	     (and beg end (buffer-substring beg end))))))
    (and str
	 (erl-company-html-to-string str))))

(defun erl-company-html-to-string (string)
  (let ((replaces '(("</?p>" . "\n")
		    ("<br>" . "\n")
		    ("<[^>]*>" . "")
		    ("[[:space:]|\n]$" . "")
		    ("^[[:space:]]+" . "")
		    ("^\n[\n]+" . "")
		    ("&gt;" . ">")
		    ("&lt;" . "<"))))
    (dolist (tagpair replaces string)
      (setq string (replace-regexp-in-string (car tagpair) (cdr tagpair) string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Distel funs       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-company-complete (search-string buf)
  "Complete search-string as a module or function in current buffer."
  (let* ((isok (string-match ":" search-string))
	 (mod (and isok (substring search-string 0 isok)))
	 (fun (and isok (substring search-string (+ isok 1))))
	 (node erl-nodename-cache))
    (if isok (erl-complete-function mod fun)
      (erl-complete-module search-string))
    (sleep-for 0.1)
    (mapcar (lambda (item) (concat mod (when mod ":") item))
	    try-erl-complete-cache)))

(defvar try-erl-args-cache '())
(defvar try-erl-desc-cache "")
(defvar try-erl-complete-cache '())

(defun erl-company-get-metadoc (mod fun)
  "Get the arguments for a function."
  (let ((node erl-nodename-cache))
    (erl-company-args mod fun))
  (sleep-for 0.1)
  try-erl-args-cache)

(defun erl-company-local-docs (mod fun)
  "Get localdocs for a function."
  (erl-company-get-metadoc mod fun)
  (let ((node erl-nodename-cache))
    (setq try-erl-desc-cache "")
    (dolist (args try-erl-args-cache)
      (erl-company-describe mod fun args)))
  (sleep-for 0.1)
  try-erl-desc-cache)

(defun erl-company-describe (mod fun args)
  (erl-spawn
    (erl-send-rpc node 'distel 'describe (list (intern mod)
					       (intern fun)
					       (length args)))
    (&erl-company-receive-describe args)))

(defun &erl-company-receive-describe (args)
  (erl-receive (args)
      ((['rex ['ok desc]]
	(let ((descr (format "%s:%s/%s\n%s\n\n"
			     (elt (car desc) 0)
			     (elt (car desc) 1)
			     (elt (car desc) 2)
			     (elt (car desc) 3))))
	(when desc (setq try-erl-desc-cache (concat descr try-erl-desc-cache)))))
       (else
	(message "fail: %s" else)))))

(defun erl-company-args (mod fun)
  (erl-spawn
    (erl-send-rpc node 'distel 'get_arglists (list mod fun))
    (&erl-company-receive-args)))

(defun &erl-company-receive-args ()
  (erl-receive ()
      ((['rex 'error])
       (['rex docs]
	(setq try-erl-args-cache docs))
       (else
	(message "fail: %s" else)
	(setq try-erl-args-cache '())))))


(defun erl-complete-module (module)
  (erl-spawn
    (erl-send-rpc node 'distel 'modules (list module))
    (&erl-complete-receive-completions)))

(defun erl-complete-function (module function)
  (erl-spawn
    (erl-send-rpc node 'distel 'functions (list module function))
    (&erl-complete-receive-completions)))


(defun &erl-complete-receive-completions ()
  (erl-receive ()
      ((['rex ['ok completions]]
	(setq try-erl-complete-cache completions))
       (other
	(message "Unexpected reply: %s" other)))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local buffer funs ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-company-grab-word ()
  "Grab the current Erlang mod/fun/word."
  (interactive)
  (buffer-substring (point) (save-excursion
			      (skip-chars-backward "a-zA-Z:_")
			      (point))))

(defun erl-company-is-comment-or-cite-p ()
  "Returns t if point is inside a comment or a cite."
  (save-excursion
    (let ((po (point)))
      (beginning-of-line)
      (re-search-forward "[%\|\"|\']" po t)
      (or (eql (char-before) ?%)
	  (and (or (eql (char-before) ?\")
		   (eql (char-before) ?\'))
	       (not (re-search-forward "[\"\|\']" po t)))))))


(provide 'company-distel-lib)
