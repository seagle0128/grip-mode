# grip-mode

[![MELPA](https://melpa.org/packages/grip-mode-badge.svg)](https://melpa.org/#/grip-mode)
[![MELPA Stable](https://stable.melpa.org/packages/grip-mode-badge.svg)](https://stable.melpa.org/#/grip-mode)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [grip-mode](#grip-mode)
    - [Prerequisite](#prerequisite)
    - [Install](#install)
        - [Manual](#manual)
        - [Use-package](#use-package)
    - [Customize](#customize)
    - [Screenshots](#screenshots)
    - [FAQ](#faq)

<!-- markdown-toc end -->

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

;; Or start grip when opening a markdown/org buffer
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

Run `M-x grip-mode` to preview the markdown and org buffer with the default browser.

Enjoy! :smile:

## Customize

Run `M-x customize-group RET grip RET` or set the variables.

``` emacs-lisp
;; Path to the grip binary
(setq grip-binary-path "/path/to/grip")

;; A GitHub username for API authentication
(setq grip-github-user "")

;; A GitHub password or auth token for API auth
(setq grip-github-password "")
```

## Screenshots

![grip-mode](https://user-images.githubusercontent.com/140797/62999172-28333480-bea0-11e9-86a3-10ef1be54c16.png
"Preview with grip")

## FAQ

1. How to resolve the issue: "GitHub Rate Limit Reached"?

   You need to set your GitHub username to `grip-github-user`, then
   [Creating a personal access token for the command
   line](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line)
   and set the new token to  `grip-github-password`.
