;;;-------------------------------------------------------------
;;; File    : company-distel.el
;;; Author  : Sebastian Weddmark Olsson
;;;           github.com/sebastiw
;;; Purpose : Backend for autocomplete-mode. To use this instead
;;;           of company-mode, add to your .emacs:
;;;           (setq ac-sources '(distel-completions))
;;; 
;;; Created : August 2012 as an internship at Klarna AB
;;; Comment : Please let me know if you find any bugs or you
;;;           want some feature or something
;;;-------------------------------------------------------------
(require 'distel)
(require 'auto-complete)
(require 'distel-completion-lib)

(defvar distel-completions
  (list '(prefix . erl-ac-get-start)
	'(candidates . (distel-completion-complete ac-prefix (current-buffer)))
	'(document . distel-completion-get-doc-string)
	'(requires . 0)
	'(symbol . "m"))
    "All it takes to start a autocomplete backend.")

(defun erl-ac-get-start ()
  "Find a valid start of a completion word."
  (save-excursion
    (let ((distance (skip-chars-backward distel-completion-valid-syntax)))
      (when (< distance 0)
        (point)))))


(provide 'auto-complete-distel)
