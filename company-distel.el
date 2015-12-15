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

(defcustom erl-company-popup-help nil
  "When set, the doc-buffer should use a popup instead of a whole buffer."
  :group 'company-distel)

(defcustom erl-company-popup-height 30
  "Specifies the height of the popup created when
`erl-company-popup-help' is set."
  :group 'company-distel)

(defvar erl-company-completion-info (make-hash-table)
  "Global variable used by frontend to determine if the
completion candidate is a module/external function, internal
function or a last used.")

;;;###autoload
(defun company-distel (command &optional args &rest ignore)
  "Backend for Company-mode using Distel."
  (company-distel-modules command args ignore))

(defun company-distel-modules (command &optional args &rest ignore)
  "Backend for company-mode using Distel to complete module-names."
  (interactive (list 'interactive))
  (case command
    (interactive
     (company-begin-backend 'company-distel-modules))
    (prefix
     ;; which word to start the completion on
     (erl-company-find-prefix))
    (candidates
     ;; returns the completion candidates
     (erl-company-get-candidates args))
    (meta
     ;; a oneline docstring
     (erl-company-get-metadoc args))
    (doc-buffer
     ;; the full documentation accessable by pressing <f1>
     (erl-company-get-help args))
    (post-completion
     ;; start completion for functions in the given module.
     ;; 1. Add a ":" after module completion, or "()" after local function
     ;;    completion, or even arguments after full mod:fun completion.
     ;; 2. If module completion, start functions-completion.

     ;;  (company-distel-functions)

     (erl-company-post-completion args)
     ;; Restart completion if it was a module that was inserted
     (when (eq (last (char-before)) ?\:) (company-manual-begin)))
    (sorted
     ;; if the list is sorted or not
     t)
    (duplicates
     ;; if there are duplicates or not; there could actually be duplicates but
     ;; we dont care about them because the list must be sorted.
     nil)
    (require-match
     ;; We want the auto-completion popup to go away if we type a word that
     ;; isn't in the list.
     nil)
    (ignore-case
     ;; Erlang uses it's cases. Turn this off.
     nil)
    (no-cache
     ;; Try to use cache.
     t)
    (t
     ;; otherwise;
     ;; one of `init', `annotation', `match', or `pre-completion'
     nil)
    ))

(defun erl-company-find-prefix ()
  "Get word at point if it is not in a comment or a cite. If it
couldn't find any return 'stop."
  (let (;; Check if point is within a comment or a citation
        (no-comment (not (distel-completion-is-comment-or-cite-p)))
        ;; Get word at point
        (word (distel-completion-grab-word)))
    (and
     ;; erlang-mode on?
     (eq (derived-mode-p 'erlang-mode) 'erlang-mode)
     (or
      ;; Not in comment/citation and we have a word
      (and no-comment word)
      ;; No word found, stop.
      'stop))))


(defun erl-company-get-candidates (prefix)
  "Return a list of completion candidates."
  (let (;; erl-dabbrevs lookback in the current function for words starting with
        ;; `prefix'
        (erl-dabbrevs (erl-get-dabbrevs prefix))
        ;; Lookup distel-completion of modules and functions
        (cc (distel-completion-complete prefix (current-buffer)))
        ;; Get functions from current-buffer
        (local-funs (erl-get-functions prefix)))

    ;; Classify each match as either 'cc or 'lu, though I have no clue what the
    ;; abbreviations mean anymore. It is used in the frontend to determine
    ;; wether to add a ":" after the completion candidate or a "(" for local
    ;; functions.
    (dolist (item cc) (puthash item 'cc erl-company-completion-info))
    (dolist (item erl-dabbrevs) (puthash item 'lu erl-company-completion-info))
    (dolist (item local-funs) (puthash item 'lu erl-company-completion-info))

    ;; Return all matches
    (append erl-dabbrevs
            local-funs
            cc)))

(defun erl-company-get-metadoc (candidate)
  "Meta-doc is a oneline documentation string, consisting of the
arguments for the function completion candidate."
  (let* ((isok (string-match ":" candidate))
         (mod (and isok (substring candidate 0 isok)))
         (fun (and isok (substring candidate (+ isok 1))))
         (met (distel-completion-get-metadoc mod fun)))
    (when isok
      (concat "Args: " (erl-format-arglists met)))))

(defun erl-company-get-help (candidate)
  "Get the company-mode's doc-buffer. If `erl-company-popup-help'
is set, then show the help as a popup instead of in a new
buffer."
  (let ((help-text (distel-completion-get-doc-buffer candidate)))
    (when erl-company-popup-help
      (unless (featurep 'popup) (require 'popup))
      (popup-tip help-text :height erl-company-popup-height))
    help-text))

(defun erl-company-post-completion (result)
  "After completion, it could be a good idea to add some extra
symbols (':', '(', etc.) and restart the completion."
  (let* ((module-end (string-match ":" result))
	 (mod (substring result 0 module-end))
	 (fun (and module-end (substring result (+ module-end 1))))
	 (arg (and fun (distel-completion-get-metadoc mod fun)))
         (info (gethash result erl-company-completion-info))
         (extra-symbols (or
                         (and
                          ;; does not include a ":"
                          (not module-end)
                          ;; and if first char is a downcase
                          (if (eq (string-to-char result)
                                  (downcase (string-to-char result)))
                              ;; it is either a local function or a module
                              (if (eql info 'lu)
                                  "("
                                ":")
                            ;; otherwise it is a variable; dont care, return
                            ""))
                         ;; if it is an external function and it has only one
                         ;; arity; expand the arguments, otherwise just add the
                         ;; starting parenthesis.
                         (or  (and (= (length arg) 1) (erl-format-arglists arg))
                              "("))))

    (insert extra-symbols)

    ;; Restart completion if it was a module that was inserted
    (when (and mod (not fun) (eql info 'cc)) (company-manual-begin))

    ;; Return inserted word
    (concat result extra-symbols)))


(provide 'company-distel)
