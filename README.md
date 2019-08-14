# grip-mode

[![MELPA](https://melpa.org/packages/grip-mode-badge.svg)](https://melpa.org/#/grip-mode)
[![MELPA Stable](https://stable.melpa.org/packages/grip-mode-badge.svg)](https://stable.melpa.org/#/grip-mode)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Github README Instant Preview ([grip](https://github.com/joeyespo/grip)) for Emacs.

## Prerequisite

- [Python](https://www.python.org/)
- [grip](https://github.com/joeyespo/grip): `pip install grip`

## Install

### Manual

From melpa, `M-x package-install RET grip-mode RET`.

Run `M-x grip-mode` to preview the markdown file with the default browser.

Or

``` emacs-lisp
;; Make a keybinding: `C-c C-c g'
(define-key markdown-mode-command-map (kbd "g") #'grip-mode)
;; or start grip when opening a markdown file
(add-hook 'markdown-mode-hook #'grip-mode)
```

### Use-package

``` emacs-lisp
(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))
```

Or `M-x grip-mode` to preview in the browser.

## Customize

Run `M-x customize-group RET grip RET` or set the variables.

``` emacs-lisp
;; Path to the grip binary
(setq grip-mode-binary-path (executable-find "grip"))
```

## Screenshots

![grip-mode](https://user-images.githubusercontent.com/140797/62999172-28333480-bea0-11e9-86a3-10ef1be54c16.png
"Preview with grip")

## FAQ
