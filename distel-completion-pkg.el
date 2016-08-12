;;; distel-completion --- Erlang/distel completion backend

;; Copyright (C) 2012 Sebastian Weddmark Olsson

;; Author:   Sebastian Weddmark Olsson
;; Created:  2012
;; URL:      http://github.com/sebastiw/distel-completion
;; Keywords: erlang distel auto-complete company-mode
;; License:  BEER-WARE

;;; Commentary:
;;
;; Auto-complete-mode
;; ------------------
;; (require 'auto-complete)
;; (require 'auto-complete-distel)
;; (add-to-list 'ac-sources 'distel-completions)
;;
;; company-mode
;; ------------------
;; (require 'company)
;; (require 'company-distel)
;; (add-to-list 'company-backends 'company-distel)
;;
;; Variables
;; ------------------
;; When set, the doc-buffer should use a popup instead of a whole buffer.
;; (setq erl-company-popup-help t)
;;
;; Which syntax to skip backwards to find start of word.
;; (setq distel-completion-get-doc-from-internet t)
;;
;; Which syntax to skip backwards to find start of word.
;; (setq distel-completion-valid-syntax "a-zA-Z:_-")
;;
;; Specifies the height of the popup created when `erl-company-popup-help' is set.
;; (setq erl-company-popup-height 30)
;;
;;; Code

(define-package "distel-completion" "1.0.0"
  "Erlang/distel completion backend for both auto-complete and company-mode")
