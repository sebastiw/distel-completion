;;; company-distel --- Erlang/distel completion backend for company-mode

;; Copyright (C) 2012 Sebastian Weddmark Olsson

;; Author:   Sebastian Weddmark Olsson
;; Homepage: http://github.com/sebastiw/distel-completion
;; Keywords: Erlang Distel company-mode
;; Created:  2012

;;; Commentary:
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
;; Specifies the height of the popup created when `erl-company-popup-help' is set.
;; (setq erl-company-popup-height 30)
;;
;; Which syntax to skip backwards to find start of word.
;; (setq distel-completion-get-doc-from-internet t)
;;
;; Which syntax to skip backwards to find start of word.
;; (setq distel-completion-valid-syntax "a-zA-Z:_-")
;;
;;; Code

(define-package "company-distel" "1.0.0"
  "Erlang/distel completion backend for company-mode"
  '((company "0.9.0")))

;;; company-distel ends here
