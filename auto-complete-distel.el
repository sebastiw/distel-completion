;;; auto-complete-distel.el --- Backend for auto-complete
;;; Commentary:
;;
;; This file contains the backend for auto-complete-mode.  To use it you need
;; to add `distel-completions' to the `ac-sources' list in your .emacs.
;; E.g.
;;   (require 'auto-complete)
;;   (require 'auto-complete-distel)
;;   (add-to-list 'ac-sources 'distel-completions)
;;
;; Please let me know if you find any bugs or you want some feature

;;; Code:

(require 'distel)
(require 'auto-complete)
(require 'distel-completion-lib)

(defvar distel-completions
  (list '(prefix . erl-ac-get-start)
	'(candidates . (distel-completion-complete ac-prefix (current-buffer)))
	'(document . distel-completion-get-doc-string)
	'(requires . 0)
	'(symbol . "m"))
    "All it takes to start a auto-complete backend.")

(defun erl-ac-get-start ()
  "Find a valid start of a completion word."
  (save-excursion
    (let ((distance (skip-chars-backward distel-completion-valid-syntax)))
      (when (< distance 0)
        (point)))))


(provide 'auto-complete-distel)
