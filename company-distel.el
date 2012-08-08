;;;-------------------------------------------------------------
;;; File    : company-distel.el
;;; Author  : Sebastian Weddmark Olsson
;;;           github.com/sebastiw
;;; Purpose : Backend for company-mode. To use it add to .emacs:
;;;           (add-to-list 'company-backends 'company-distel)
;;;           or (require 'company-distel-frontend)
;;; 
;;; Created : August 2012 as an internship at Klarna AB
;;; Comment : Please let me know if you find any bugs or you
;;;           want some feature or something
;;;-------------------------------------------------------------
(require 'company-distel-lib)

;;;###autoload
(defun company-distel (command &optional args &rest ignore)
  "Backend for Company-mode using Distel."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-distel))
    (prefix ;; nar ska functionen slas pa? returnera ordet man ar pa.
     (erl-company-find-prefix))
    (candidates ;; vilka ord returneras
     (erl-company-complete args (current-buffer)))
    (meta
     (let* ((isok (string-match ":" args))
	    (mod (and isok (substring args 0 isok)))
	    (fun (and isok (substring args (+ isok 1))))
	    (met (erl-company-get-metadoc mod fun)))
       (when isok (concat "Args: " (erl-format-arglists met)))))
    (doc-buffer
     (erl-company-get-doc-buffer args))

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

(defun erl-company-get-doc-buffer (args)
  "Returns a buffer with the documentation for ARGS."
  (let* ((isok (string-match ":" args))
	 (mod (or (and isok (substring args 0 isok))
		  args))
	 (fun (and isok (substring args (+ isok 1))))
	 (doc (and fun (erl-company-local-docs mod fun)))
	 (edocs (when (or (not doc) (string= doc "")) (erl-company-get-docs-from-internet-p mod fun)))
	 (met (and fun (erl-format-arglists (erl-company-get-metadoc mod fun))))
	 (to-show (or (and (not (string= doc "")) doc)
		      (and (not (string= edocs "")) edocs)
		      (concat mod ":" fun met)
		      (format "Couldn't find any help for %s" args))))

    (with-current-buffer (company-doc-buffer)
      (insert to-show)
      (current-buffer))))


(provide 'company-distel)
