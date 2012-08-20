;;;-------------------------------------------------------------
;;; File    : company-distel-frontend.el
;;; Author  : Sebastian Weddmark Olsson
;;;           github.com/sebastiw
;;; Purpose : Not really a frontend, just replaces the function
;;;           company-finish to start again after a completion.
;;;           Also to have the helpdocs appear in a popup instead
;;;           of a new buffer.
;;; 
;;; Created : August 2012 as an internship at Klarna AB
;;; Comment : Please let me know if you find any bugs or you
;;;           want some feature or something
;;;-------------------------------------------------------------
(require 'company)
(require 'distel)
(require 'popup)

(defun erl-company-help ()
  "If help text is activated, show it in a popup-tip instead of a new buffer."
  (interactive)
    (let* ((selected (nth company-selection company-candidates))
	   (doc-buffer (or
			(company-call-backend 'doc-buffer selected)
			(error "No documentation available"))))
      (popup-tip doc-buffer :height 30)))

(defun company-finish (result)
  "Overwrites company-finish to add some extra symbols and restart the completion."
  (let* ((ismod (string-match ":" result))
	 (mod (substring result 0 ismod))
	 (fun (and ismod (substring result (+ ismod 1))))
	 (arg (and fun (erl-company-get-metadoc mod fun)))
	 (str (company-strip-prefix result))
	 (inf (gethash result erl-company-completion-info)))

    ;; insert result
    (insert (or
	     (and (not ismod)
		  ;; if first char is a downcase
		  (if (eq (string-to-char result)
			  (downcase (string-to-char result)))
		      ;; either it is a local function or a module
		      (if (eql inf 'lu)
			  (concat str "(")
			(concat str ":"))
		    ;; else it is a variable and then just return it
		    str))
	     ;; if it is an external function and it has only one arity 
	     (and (= (length arg) 1) (concat str (erl-format-arglists arg)))
	     str))

    (company-cancel result)

    ;; set point to the new position
    (setq company-point (or (and ismod (+ (point) 1))
			    (point)))

    ;; if it is a module, start the completion again
    (when (and mod (not fun) (eql inf 'cc)) (company-manual-begin))))

(provide 'company-distel-frontend)
