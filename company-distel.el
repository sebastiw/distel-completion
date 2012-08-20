;;;-------------------------------------------------------------
;;; File    : company-distel.el
;;; Author  : Sebastian Weddmark Olsson
;;;           github.com/sebastiw
;;; Purpose : Backend for company-mode. To use it add to .emacs:
;;;           (add-to-list 'company-backends 'company-distel)
;;; 
;;; Created : August 2012 as an internship at Klarna AB
;;; Comment : Please let me know if you find any bugs or you
;;;           want some feature or something
;;;-------------------------------------------------------------
(require 'distel-completion-lib)

(defvar erl-company-completion-info (make-hash-table)
  "Global variable used by frontend to determine if the completion
candidate is a module/external function, internal function or a last used.")

;;;###autoload
(defun company-distel (command &optional args &rest ignore)
  "Backend for Company-mode using Distel."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-distel))
    (prefix ; which word to start the completion on
     (erl-company-find-prefix))
    (candidates ; returns the completion candidates
     (let ((erl-dabbrevs (erl-get-dabbrevs args))
	   (cc (erl-company-complete args (current-buffer)))
	   (local-funs (erl-get-functions args)))
       
       (dolist (item cc) (puthash item 'cc erl-company-completion-info))
       (dolist (item erl-dabbrevs) (puthash item 'lu erl-company-completion-info))
       (dolist (item local-funs) (puthash item 'lu erl-company-completion-info))
       
       (append erl-dabbrevs
	       local-funs
	       cc)))

    (meta ; a oneline docstring
     (let* ((isok (string-match ":" args))
	    (mod (and isok (substring args 0 isok)))
	    (fun (and isok (substring args (+ isok 1))))
	    (met (erl-company-get-metadoc mod fun)))
       (when isok (concat "Args: " (erl-format-arglists met)))))
    (doc-buffer ; the full documentation accessable by pressing <f1>
     (erl-company-get-doc-buffer args))
    (sorted t) ; if the list is sorted or not
    (duplicates nil) ; if there is duplicates or not;
		     ; there could actually be duplicates but we dont care about them because the list must be sorted.

    (t ;(message "(%s):%s" command args)
       nil)))


(defun erl-company-find-prefix ()
  "Get word at point if it is not in a comment or a cite. If it couldn't find any return 'stop."
  (let ((no-comment (not (erl-company-is-comment-or-cite-p)))
	(word (erl-company-grab-word)))
    (and
     (eq (derived-mode-p 'erlang-mode) 'erlang-mode)
     (or
      (and no-comment word)
      'stop))))


(provide 'company-distel)
