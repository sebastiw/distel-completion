
(require 'distel)
(require 'auto-complete)
(require 'distel-completion-lib)

(defvar distel-completions
  (list '(prefix . erl-ac-get-start)
	'(candidates . (erl-company-complete ac-prefix (current-buffer)))
	'(document . erl-company-get-doc-string)
	'(requires . 0)
	'(symbol . "m")))

(defvar erl-distel-valid-syntax "a-zA-Z:_"
  "Which syntax to skip backwards to find start of word.")

(defun erl-ac-get-start ()
  (save-excursion
    (skip-chars-backward erl-distel-valid-syntax)
    (point)))


(provide 'auto-complete-distel)
