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

(defcustom distel-completion-get-doc-from-internet t
  "Try to find the documentation from erlang.org"
  :group 'distel-completion)

(defcustom distel-completion-valid-syntax "a-zA-Z:_-"
  "Which syntax to skip backwards to find start of word."
  :group 'distel-completion)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; docs funs         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun distel-completion-get-doc-buffer (args)
  "Returns a buffer with the documentation for ARGS."
  (with-current-buffer (company-doc-buffer)
    (insert (distel-completion-get-doc-string args))
    (current-buffer)))

(defun distel-completion-get-doc-string (args)
  "Returns the documentation string for ARGS."
  (interactive)
  (let* ((isok (string-match ":" args))
	 (mod (substring args 0 isok))
	 (fun (and isok (substring args (+ isok 1))))
	 (doc (and fun (distel-completion-local-docs mod fun)))
	 (edocs (when (and distel-completion-get-doc-from-internet
                           mod
                           (or (not doc) (string= doc "")))
		  (distel-completion-get-docs-from-internet-p mod fun)))
	 (met (and fun (erl-format-arglists (distel-completion-get-metadoc mod fun))))
	 (to-show (or (and (not (string= doc "")) doc)
		      (and (not (string= edocs "")) edocs)
		      (and met (concat mod ":" fun met))
		      (format "Couldn't find any help for %s." args))))
    to-show))

(defun distel-completion-get-docs-from-internet-p (mod fun) ;; maybe version?
  "Download the documentation from internet."
  (let ((str
	 (with-current-buffer 
	     (url-retrieve-synchronously (format "http://www.erlang.org/doc/man/%s.html" mod))
	   (goto-char (point-min))
	   
	   ;; find <p> containing <a name="`FUN'"> then
	   ;; find <div class="REFBODY">
	   (let* ((m (re-search-forward (or (and fun (format "<p>.*?<a name=\"%s.*?\">" fun))
					    "<h3>DESCRIPTION</h3>") nil t))
		  (beg (and m (match-end 0)))
		  (end (and m (progn (re-search-forward "</p>.*?</div>" nil t)
				     (match-end 0)))))
	     (and beg end (buffer-substring beg end))))))
    (and str
	 (distel-completion-html-to-string str))))

(defun distel-completion-html-to-string (string)
  "Removes html-tags from `STRING'."
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

(defun distel-completion-complete (search-string buf)
  "Complete `search-string' as a external module or function in current buffer."
  (let* (;; Checks if there exists a ":" in the search-string
         (isok (string-match ":" search-string))
         ;; module (from start of string to ":")
         (mod (and isok (substring search-string 0 isok)))
         ;; function (from ":" to end of string)
         (fun (and isok (substring search-string (+ isok 1))))
         ;; node
         (node erl-nodename-cache))
    ;; Let distel complete the function or module, depending if ":" is part of
    ;; the search-string
    (if isok
        (erl-complete-function mod fun)
      (erl-complete-module search-string))
    ;; The call to distel is asynchronous, lets wait some
    (sleep-for 0.1)
    ;; Add the module if needed
    (mapcar (lambda (item) (concat mod (when mod ":") item))
            try-erl-complete-cache)))

(defvar try-erl-args-cache '()
  "Store the functions arguments.")
(defvar try-erl-desc-cache ""
  "Store of the description.")
(defvar try-erl-complete-cache '()
  "Completion candidates cache.")

(defun distel-completion-get-metadoc (mod fun)
  "Get the arguments for a function."
  (let ((node erl-nodename-cache))
    (distel-completion-args mod fun))
  (sleep-for 0.1)
  try-erl-args-cache)

(defun distel-completion-local-docs (mod fun)
  "Get local documentation for a function."
  (distel-completion-get-metadoc mod fun)
  (let ((node erl-nodename-cache))
    (setq try-erl-desc-cache "")
    (dolist (args try-erl-args-cache)
      (distel-completion-describe mod fun args)))
  (sleep-for 0.1)
  try-erl-desc-cache)

(defun distel-completion-describe (mod fun args)
  "Get the documentation of a function."
  (erl-spawn
    (erl-send-rpc node 'distel 'describe (list (intern mod)
					       (intern fun)
					       (length args)))
    (&distel-completion-receive-describe args)))

(defun &distel-completion-receive-describe (args)
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

(defun distel-completion-args (mod fun)
  "Find the arguments to a function `FUN' in a module `MOD'"
  (erl-spawn
    (erl-send-rpc node 'distel 'get_arglists (list mod fun))
    (&distel-completion-receive-args)))

(defun &distel-completion-receive-args ()
  (erl-receive ()
      ((['rex 'error])
       (['rex docs]
	(setq try-erl-args-cache docs))
       (else
	(message "fail: %s" else)
	(setq try-erl-args-cache '())))))


(defun erl-complete-module (module)
  "Get list of modulenames starting with `MODULE'."
  (erl-spawn
    (erl-send-rpc node 'distel 'modules (list module))
    (&erl-complete-receive-completions)))

(defun erl-complete-function (module function)
  "Get list of function names starting with `FUNCTION'"
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

(defun erl-get-dabbrevs (args &optional times limit)
  "Use `dabbrev' to find last used commands beginning with ARGS
in current buffer only. TIMES is how many times it will match,
defaults to 5. LIMIT is how far from point it should search,
defaults to the start of the previous defun."
  (when (require 'dabbrev nil t)
    (dabbrev--reset-global-variables)
    (setq dabbrev-check-other-buffers nil
          dabbrev--last-expansion-location (point))
    (let ((times (or times 5))
          (dabbrev-limit (or limit (- (point) (save-excursion
                                                (backward-char (length args))
                                                (backward-paragraph)
                                                (point)))))
          (ret-list '())
          str)

      (while (and (> times 0) (dabbrev--find-expansion args 1 nil)))
      dabbrev--last-table)))

(defun erl-get-functions (args)
  "Get all function definitions beginning with ARGS in the current buffer."
  (let ((case-fold-search nil)
        (funs '())
        fun-end)
    (save-excursion
      (goto-char (point-min))
      ;; Search for functions in the current-buffer starting with `args'
      (while (setq fun-end (re-search-forward (format "^%s.*?(" args) nil t))
        ;; add found functions as completion candidates
        (add-to-list 'funs (buffer-substring (match-beginning 0) (1- fun-end)))))
    funs))

(defun distel-completion-grab-word ()
  "Grab the current Erlang mod/fun/word."
  (interactive)
  (buffer-substring (point) (save-excursion
			      (skip-chars-backward distel-completion-valid-syntax)
			      (point))))

(defun distel-completion-is-comment-or-cite-p (&optional poin)
  "Returns t if point is inside a comment or a cite."
  (save-excursion
    (let ((po (or poin (point))))
      (goto-char po)
      (beginning-of-line)
      (re-search-forward "[%\|\"|\']" po t)
      (or (eql (char-before) ?%)
	  (and (or (eql (char-before) ?\")
		   (eql (char-before) ?\'))
	       (not (re-search-forward "[\"\|\']" po t)))))))


(provide 'distel-completion-lib)
