# Distel-complete

Distel-complete is a completion module for both
* `auto-complete-mode`
* `company-mode`
using Distel as a backend.

## Usage:
### with auto-complete-mode
```lisp
(require 'auto-complete-distel)
(setq ac-sources '(distel-completions))
```

### with company-mode
```
(require 'company-distel)
(add-to-list 'company-backends 'company-distel)
```

There is also an optional parameter which can be modified.
```(setq erl-company-popup-help t)```

This will render company's doc-buffer (default <F1>) in a small popup (using
popup.el) instead of showing the whole help-buffer.
