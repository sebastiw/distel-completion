;;; auto-complete-distel --- Erlang/distel completion backend for auto-complete-mode

;; Copyright (C) 2012 Sebastian Weddmark Olsson

;; Author:   Sebastian Weddmark Olsson
;; Created:  2012
;; Keywords: Erlang Distel auto-complete-mode
;; Homepage: http://github.com/sebastiw/distel-completion

;;; Commentary:
;;
;; Auto-complete-mode
;; ------------------
;; (require 'auto-complete)
;; (require 'auto-complete-distel)
;; (add-to-list 'ac-sources 'distel-completions)
;;
;; Variables
;; ------------------
;; Which syntax to skip backwards to find start of word.
;; (setq distel-completion-get-doc-from-internet t)
;;
;; Which syntax to skip backwards to find start of word.
;; (setq distel-completion-valid-syntax "a-zA-Z:_-")
;;
;;; Code

(define-package "auto-complete-distel" "1.0.0"
  "Erlang/distel completion backend for auto-complete-mode"
  '((auto-complete "1.4")))

;;; auto-complete-distel ends here
