# grip-mode

[![MELPA](https://melpa.org/packages/grip-mode-badge.svg)](https://melpa.org/#/grip-mode)
[![MELPA Stable](https://stable.melpa.org/packages/grip-mode-badge.svg)](https://stable.melpa.org/#/grip-mode)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

Realtime Markdown previews for Emacs, with a grip subprocess.

## Install

### Manual

From melpa, `M-x package-install RET grip-mode RET`.

Or download `grip-mode.el`, and add `(require 'grip-mode)` to the configurations.

### Use-package

``` emacs-lisp
(use-package grip-mode
  :ensure t)
```

Then `M-x grip-mode` to preview the markdown file with the default browser.

## Customize

Run `M-x customize-group RET grip RET` or set the variables.

``` emacs-lisp
;; Path to the grip binary
(setq grip-mode-binary-path (executable-find "grip"))
```

## Screenshots

## FAQ
