;;; grip-mode.el --- Instant GitHub-flavored Markdown/Org preview using grip.        -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/grip-mode
;; Version: 2.3.3
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, markdown, preview

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:

;; Instant GitHub-flavored Markdown/Org preview using a grip subprocess.
;;
;; Install:
;; From melpa, `M-x package-install RET grip-mode RET`.
;; ;; Make a keybinding: `C-c C-c g'
;; (define-key markdown-mode-command-map (kbd "g") #'grip-mode)
;; ;; or start grip when opening a markdown file
;; (add-hook 'markdown-mode-hook #'grip-mode)
;; or
;; (use-package grip-mode
;;   :ensure t
;;   :bind (:map markdown-mode-command-map
;;          ("g" . grip-mode)))
;; Run `M-x grip-mode` to preview the markdown file with the default browser.

;;; Code:

(defgroup grip nil
  "Instant GitHub-flavored Markdown/Org preview using grip."
  :group 'markdown
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/grip-mode"))

(defcustom grip-binary-path "grip"
  "Path to the grip binary."
  :type 'file
  :group 'grip)

(defcustom grip-mdopen-path "mdopen"
  "Path to the mdopen binary."
  :type 'file
  :group 'grip)

(defcustom grip-use-mdopen nil
  "Use mdopen instead of grip if non-nil."
  :type 'boolean
  :group 'grip)

(defcustom grip-use-gogrip nil
  "Use go-grip instead of grip if non-nil."
  :type 'boolean
  :group 'grip)

(defcustom grip-gogrip-path "go-grip"
  "Path to the go-grip binary."
  :type 'file
  :group 'grip)

(defcustom grip-gogrip-theme "auto"
  "Theme choice for go-grip."
  :type '(choice
          (const :tag "Automatic" "auto")
          (const :tag "Dark" "dark")
          (const :tag "Light" "light"))
  :group 'grip)

(defcustom grip-preview-use-webkit t
  "Use embedded webkit to preview.

This requires Emacs GUI version >= 26 and built with the `--with-xwidgets`
option."
  :type 'boolean
  :group 'grip)

(defcustom grip-url-browser nil
  "Browser to launch Markdown/Org previews.
Use default browser if nil. It respects `grip-preview-use-webkit'."
  :type '(choice (const :tag "None" nil) string)
  :group 'grip)

(defcustom grip-url-args nil
  "A list of strings defining options for `grip-url-browser'."
  :type '(repeat (string :tag "Argument")))

(defcustom grip-github-api-url ""
  "A base URL to another GitHub API."
  :type 'string
  :group 'grip)

(defcustom grip-github-user ""
  "A GitHub username for API authentication."
  :type 'string
  :group 'grip)

(defcustom grip-github-password ""
  "A GitHub password or auth token for API auth."
  :type 'string
  :group 'grip)

(defcustom grip-update-after-change t
  "Update the grip review after every text change when non-nil.

When nil, only update the preview on file save."
  :type 'boolean
  :group 'grip)

(defcustom grip-preview-host "localhost"
  "Preview hostname."
  :type 'string
  :group 'grip)

(defcustom grip-sleep-time 2
  "Sleep seconds to ensure the server starts successfully."
  :type 'integer
  :group 'grip)



;; Externals
(declare-function xwidget-buffer "xwidget")
(declare-function xwidget-webkit-browse-url "xwidget")
(declare-function xwidget-webkit-current-session "xwidget")
(declare-function xwidget-webkit-current-url "xwidget")

(defvar-local grip--process nil
  "Handle to the inferior grip process.")

(defvar-local grip--port 6418
  "Port to the grip port.")

(defvar-local grip--preview-file nil
  "The preview file for grip process.")

(defun grip--browser (url)
  "Use browser specified by user to load URL.
Use default browser if nil."
  (if grip-url-browser
      (let ((browse-url-generic-program grip-url-browser)
            (browse-url-generic-args grip-url-args))
        (browse-url-generic url))
    (browse-url url)))

(defun grip--browse-url (url)
  "Ask the browser to load URL.

Use default browser unless `xwidget' is available."
  (if (and grip-preview-use-webkit
           (display-graphic-p)
           (featurep 'xwidget-internal))
      (save-selected-window
        (xwidget-webkit-browse-url url)
        (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
          (when (buffer-live-p buf)
            (and (eq buf (current-buffer)) (quit-window))
            (pop-to-buffer buf))))
    (grip--browser url)))

(defun grip--preview-url ()
  "Return grip preview url."
  (if grip-use-gogrip
      (format "http://%s:%d/%s" grip-preview-host grip--port
              (file-name-nondirectory grip--preview-file))
    (format "http://%s:%d" grip-preview-host grip--port)))

(defun grip-start-process ()
  "Render and preview with grip or mdopen."
  (unless (processp grip--process)
    (cond (grip-use-mdopen
           (progn
             (unless (and grip-mdopen-path (executable-find grip-mdopen-path))
               (grip-mode -1)                    ; Force to disable
               (user-error "The `mdopen' is not available in PATH environment"))
             (when grip--preview-file
               (setq grip--process
                     (start-process "mdopen" "*mdopen*"
                                    grip-mdopen-path
                                    grip--preview-file))
               (message "Preview `%s' on %s" buffer-file-name (grip--preview-url)))))
          (grip-use-gogrip
           (progn
             (unless (and grip-gogrip-path (executable-find grip-gogrip-path))
               (grip-mode -1)                    ; Force to disable
               (user-error "The `go-grip' is not available in PATH environment"))
             ;; Generate random port
             (while (< grip--port 6419)
               (setq grip--port (random 65535)))
             ;; Start a new grip process
             (when grip--preview-file
               (setq grip--process
                     (start-process (format "grip-%d" grip--port)
                                    (format " *grip-%d*" grip--port)
                                    grip-gogrip-path
                                    (format "--port=%d" grip--port)
                                    (format "--theme=%s" grip-gogrip-theme)
                                    "--browser=false"
                                    grip--preview-file))
               (message "Preview `%s' on %s" buffer-file-name (grip--preview-url))
               (sleep-for grip-sleep-time)
               (grip--browse-url (grip--preview-url)))))
          (t
           (progn
             (unless (and grip-binary-path (executable-find grip-binary-path))
               (grip-mode -1)                    ; Force to disable
               (user-error "The `grip' is not available in PATH environment"))
             ;; Generate random port
             (while (< grip--port 6419)
               (setq grip--port (random 65535)))
             ;; Start a new grip process
             (when grip--preview-file
               (setq grip--process
                     (start-process (format "grip-%d" grip--port)
                                    (format " *grip-%d*" grip--port)
                                    grip-binary-path
                                    (format "--api-url=%s" grip-github-api-url)
                                    (format "--user=%s" grip-github-user)
                                    (format "--pass=%s" grip-github-password)
                                    (format "--title=%s - Grip" (buffer-name))
                                    grip--preview-file
                                    (number-to-string grip--port)))
               (message "Preview `%s' on %s" buffer-file-name (grip--preview-url))
               (sleep-for grip-sleep-time)
               (grip--browse-url (grip--preview-url))))))))

(defun grip--kill-process ()
  "Kill grip or mdopen or go-grip process."
  (when grip--process
    ;; Delete xwidget buffer
    (when (and grip-preview-use-webkit
               (display-graphic-p)
               (featurep 'xwidget-internal)
               (xwidget-webkit-current-session)
               (string-match-p (grip--preview-url) (xwidget-webkit-current-url)))
      (let ((kill-buffer-query-functions nil)
            (buf (xwidget-buffer (xwidget-webkit-current-session))))
        (when (buffer-live-p buf)
          (kill-buffer buf))))
    ;; Delete process
    (delete-process grip--process)
    (message "Process `%s' killed" grip--process)
    (setq grip--process nil)
    (setq grip--port 6418)
    ;; Delete preview temporary file
    (when (and grip--preview-file
               (not (string-equal grip--preview-file buffer-file-name)))
      (delete-file grip--preview-file))))

(defun grip-refresh-md (&rest _)
  "Refresh the contents of `grip--preview-file'."
  (when (and grip--preview-file
             (file-writable-p grip--preview-file))
    (write-region nil nil grip--preview-file nil 'quiet)))

(defun grip--preview-md ()
  "Render and preview markdown with grip."
  (when grip-update-after-change
    (add-hook 'after-change-functions #'grip-refresh-md nil t))
  (add-hook 'after-save-hook #'grip-refresh-md nil t)
  (add-hook 'after-revert-hook #'grip-refresh-md nil t)

  (setq grip--preview-file (concat (file-name-sans-extension buffer-file-name) ".tmp.md"))
  (grip-refresh-md)
  (grip-start-process))

(defun grip-org-to-md (&rest _)
  "Render org to markdown."
  (cond
   ((fboundp 'org-gfm-export-to-markdown)
    (org-gfm-export-to-markdown))
   ((fboundp 'org-md-export-to-markdown)
    (org-md-export-to-markdown))
   (t
    (user-error "Unable to export org to markdown"))))

(defun grip--preview-org ()
  "Render and preview org with grip."
  ;; (when grip-update-after-change
  ;;   (add-hook 'after-change-functions #'grip-org-to-md nil t))
  (add-hook 'after-save-hook #'grip-org-to-md nil t)
  (add-hook 'after-revert-hook #'grip-org-to-md nil t)

  (setq grip--preview-file (expand-file-name (grip-org-to-md)))
  (grip-start-process))

(defun grip-start-preview ()
  "Start rendering and previewing with grip."
  (interactive)
  (when buffer-file-name
    (add-hook 'kill-buffer-hook #'grip-stop-preview nil t)
    (add-hook 'kill-emacs-hook #'grip-stop-preview nil t)
    (cond ((derived-mode-p 'org-mode)
           (grip--preview-org))
          ((derived-mode-p 'markdown-mode 'markdown-ts-mode)
           (grip--preview-md))
          (t
           (grip-mode -1)
           (user-error "`%s' not supported by grip preview" major-mode)))))

(defun grip-stop-preview ()
  "Stop rendering and previewing with grip."
  (interactive)
  ;; Remove hooks
  ;; (remove-hook 'after-change-functions #'grip-org-to-md t)
  (remove-hook 'after-save-hook #'grip-org-to-md t)
  (remove-hook 'after-revert-hook #'grip-org-to-md t)
  (remove-hook 'after-change-functions #'grip-refresh-md t)
  (remove-hook 'after-save-hook #'grip-refresh-md t)
  (remove-hook 'after-revert-hook #'grip-refresh-md t)
  (remove-hook 'kill-buffer-hook #'grip-stop-preview t)
  (remove-hook 'kill-emacs-hook #'grip-stop-preview t)

  ;; Kill grip process
  (grip--kill-process))

(defun grip-restart-preview ()
  "Restart grip process to preview."
  (interactive)
  (grip-stop-preview)
  (grip-start-preview))

(defun grip-browse-preview ()
  "Browse grip preview."
  (interactive)
  (if grip--process
      (grip--browse-url (grip--preview-url))
    (grip-start-preview)))

;;;###autoload
(define-minor-mode grip-mode
  "Live Markdown preview with grip."
  :lighter " grip"
  (if grip-mode
      (grip-start-preview)
    (grip-stop-preview)))

(provide 'grip-mode)

;;; grip-mode.el ends here
