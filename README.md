# grip-mode

[![MELPA](https://melpa.org/packages/grip-mode-badge.svg)](https://melpa.org/#/grip-mode)
[![MELPA Stable](https://stable.melpa.org/packages/grip-mode-badge.svg)](https://stable.melpa.org/#/grip-mode)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Instant Github-flavored Markdown/Org preview using [grip](https://github.com/joeyespo/grip)
(Github README Instant Preview).

## Prerequisite

- [Python](https://www.python.org/)
- [grip](https://github.com/joeyespo/grip): `pip install grip`

## Install

### Manual

From melpa, `M-x package-install RET grip-mode RET`.

``` emacs-lisp
;; Make a keybinding: `C-c C-c g'
(define-key markdown-mode-command-map (kbd "g") #'grip-mode)

;; Or start grip when opening a markdown/org file
(add-hook 'markdown-mode-hook #'grip-mode)
(add-hook 'org-mode-hook #'grip-mode)
```

### Use-package

``` emacs-lisp
;; Use keybindings
(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

;; Or using hooks
(use-package grip-mode
  :ensure t
  :hook ((markdown-mode org-mode) . grip-mode))
```

Run `M-x grip-mode` to preview the markdown and org file with the default browser.

Enjoy! :smile:

## Customize

Run `M-x customize-group RET grip RET` or set the variables.

``` emacs-lisp
;; Path to the grip binary
(setq grip-binary-path "/path/to/grip")
```

## Screenshots

![grip-mode](https://user-images.githubusercontent.com/140797/62999172-28333480-bea0-11e9-86a3-10ef1be54c16.png
"Preview with grip")

## Limitations

- Need to save the buffer before rendering and previewing.
- After reverting the buffer, the grip process will be killed automatically.

## FAQ
