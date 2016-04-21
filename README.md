# Distel-complete

Distel-complete is a completion module for both
* `auto-complete-mode`
* `company-mode`
using Distel as a backend.

## Usage:
### with auto-complete-mode
```elisp
(require 'auto-complete-distel)
(add-hook 'erlang-mode-hook '(lambda () (add-to-list 'ac-sources 'distel-completions)))
```

### with company-mode
```elisp
(require 'company-distel)
(add-to-list 'company-backends 'company-distel)
```

There is also an optional parameter which can be modified.
```elisp
(setq erl-company-popup-help t)
```

This will render company's doc-buffer (default &lt;F1&gt; when on a
completion-candidate) in a small popup (using popup.el) instead of showing the
whole help-buffer.

## Extras
### Get Erlang-docs from internet
Get the documentation from the internet:
```elisp
(setq distel-completion-get-doc-from-internet t)
```

### Change completion symbols
Change the allowed symbols to skip backwords in order to find
start-of-word. (showing defaults)
```elisp
(setq distel-completion-valid-syntax "a-zA-Z:_-")
```

## Current issues
1. fail: badrpc

```
fail: [rex [badrpc [EXIT [undef ([distel describe (io format 3) nil]
[rpc -handle_call_call/6-fun-0- 5 ([file rpc.erl] [line 206])])]]]]
```

This error comes when trying to get the documentation for a completion
candidate. It actually uses same functionality as `erl-fdoc-describe`
(C-c C-d d), and if this function stops at "Sent request; waiting for
results..", it is the same fault.

2. In company-mode restart complletion after completing a module.

3. Show whether the completion candidate is of a module or a local function.

4. Add some formating to documentation-buffer.


