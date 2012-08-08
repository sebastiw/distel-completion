(require 'company)
(require 'distel)
(require 'popup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company

(defun erl-company-help ()
  (interactive)
    (let* ((selected (nth company-selection company-candidates))
	   (doc-buffer (or
			(company-call-backend 'doc-buffer selected)
			(error "No documentation available"))))
      (popup-tip doc-buffer :height 30)
))

(defun company-finish (result)
  (let* ((isok (string-match ":" result))
	 (mod (and isok (substring result 0 isok)))
	 (fun (and isok (substring result (+ isok 1))))
	 (arg (and fun (erl-company-get-metadoc mod fun)))
	 (str (company-strip-prefix result)))
    (insert (or (and (not isok) (concat str ":"))
		(and (= (length arg) 1) (concat str (erl-format-arglists arg)))
		str))
    (when isok (company-cancel result))
    (setq company-point (or (and (not isok) (+ (point) 1))
			    (point)))))

(defun company-distel-setup ()
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay .2)
  (add-to-list 'company-backends 'company-distel)
  (define-key company-active-map "\C-n" 'company-select-next)
  (define-key company-active-map "\C-p" 'company-select-previous)
  (define-key company-active-map [remap company-show-doc-buffer] 'erl-company-help)
;;  (require 'company-distel-frontend)
;  (setq company-frontends (list 'company-distel-frontend))
  (company-mode))

(add-hook 'erlang-mode-hook 'company-distel-setup)



(provide 'company-distel-frontend)
