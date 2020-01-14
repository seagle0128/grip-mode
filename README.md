# grip-mode

[![MELPA](https://melpa.org/packages/grip-mode-badge.svg)](https://melpa.org/#/grip-mode)
[![MELPA Stable](https://stable.melpa.org/packages/grip-mode-badge.svg)](https://stable.melpa.org/#/grip-mode)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](LICENSE)

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

Instant Github-flavored Markdown/Org preview using [Grip](https://github.com/joeyespo/grip)
(GitHub Readme Instant Preview).

## Prerequisite

- [Python](https://www.python.org/)
- [Grip](https://github.com/joeyespo/grip): `pip install grip`

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

;; Use embedded webkit to preview
(setq grip-preview-use-webkit t)
```

## Screenshots

![default_browser](https://user-images.githubusercontent.com/140797/62999172-28333480-bea0-11e9-86a3-10ef1be54c16.png
"Preview in browser")

![xwidget_webkit](https://user-images.githubusercontent.com/140797/72371426-52369e80-373f-11ea-920a-5b6154852c57.png
"Preview in embedded webkit")


## FAQ

1. How to resolve the issue: "GitHub Rate Limit Reached"?

   You need to set your GitHub username to `grip-github-user`, then
   [Creating a personal access token for the command
   line](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line)
   and set the new token to  `grip-github-password`.

## Donate

If you think it's helpful for you, please considering pay a cup of coffee for me. Thank you! :smile:

<img
src="https://user-images.githubusercontent.com/140797/65818854-44204900-e248-11e9-9cc5-3e6339587cd8.png"
alt="Alipay" width="120"/>
&nbsp;&nbsp;&nbsp;&nbsp;
<img
src="https://user-images.githubusercontent.com/140797/65818844-366ac380-e248-11e9-931c-4bd872d0566b.png"
alt="Wechat Pay" width="120"/>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://paypal.me/seagle0128" target="_blank">
<img
src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/Online_Primary_Acceptance_Mark_RGB_V2_medium.jpg"
alt="Paypal"/>
</a>
