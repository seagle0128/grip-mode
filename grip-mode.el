;;; grip-mode.el --- Instant GitHub-flavored Markdown/Org preview using grip.        -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2025 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/grip-mode
;; Version: 2.5.1
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

(defcustom grip-command 'auto
  "The command of grip."
  :type '(choice
          (const :tag "Automatic" auto)
          (const :tag "Grip" grip)
          (const :tag "Go-grip" go-grip)
          (const :tag "Mdopen" mdopen))
  :group 'grip)

(defcustom grip-theme 'auto
  "Theme choice."
  :type '(choice
          (const :tag "Automatic" auto)
          (const :tag "Dark" dark)
          (const :tag "Light" light))
  :group 'grip)

(defcustom grip-preview-use-webkit t
  "Use embedded webkit to preview.

This requires Emacs GUI version >= 26 and built with the `--with-xwidgets`
option. mdopen doesn't support webkit preview."
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

(defcustom grip-github-api-url "https://api.github.com"
  "A base URL to another GitHub API.
Only available for `grip'."
  :type 'string
  :group 'grip)

(defcustom grip-github-user ""
  "A GitHub username for API authentication.
Only available for `grip'."
  :type 'string
  :group 'grip)

(defcustom grip-github-password ""
  "A GitHub password or auth token for API auth.
Only available for `grip'."
  :type 'string
  :group 'grip)

(defcustom grip-preview-host "localhost"
  "Preview hostname.
Only available for `grip'."
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

(defvar browse-url-generic-args)
(defvar browse-url-generic-program)

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

(defvar grip--command nil)
(defun grip--preview-url ()
  "Return grip preview url."
  (if (eq grip--command 'grip)
      (format "http://%s:%d" grip-preview-host grip--port)
    (format "http://%s:%d/%s" grip-preview-host grip--port
            (file-name-nondirectory grip--preview-file))))

(defun grip-start-process ()
  "Render and preview."
  (unless (process-live-p grip--process)
    (setq grip--command grip-command)
    (when (eq grip--command 'auto)
      (setq grip--command
            (cond
             ((executable-find "mdopen") 'mdopen)
             ((executable-find "go-grip") 'go-grip)
             ((executable-find "grip") 'grip)
             (t (user-error "No grip command is available in PATH environment")))))

    ;; Generate random port
    (while (< grip--port 6419)
      (setq grip--port (random 65535)))

    (pcase grip--command
      ('mdopen
       (unless (executable-find "mdopen")
         (grip-mode -1)
         (user-error "The `mdopen' is not available in PATH environment"))

       (when grip--preview-file
         (setq grip--process
               (start-process (format "mdopen-%d" grip--port)
                              (format " *mdopen-%d*" grip--port)
                              "mdopen"
                              (format "--port=%d" grip--port)
                              "--browser="
                              "--reload"
                              (format "%s.md" (file-name-base grip--preview-file))))

         (when (process-live-p grip--process)
           (grip--browse-url (grip--preview-url))
           (message "%s: Preview `%s' on %s"
                    grip--command
                    (abbreviate-file-name buffer-file-name)
                    (grip--preview-url)))))
      ('go-grip
       (unless (executable-find "go-grip")
         (grip-mode -1)
         (user-error "The `go-grip' is not available in PATH environment"))

       (when grip--preview-file
         (setq grip--process
               (start-process (format "go-grip-%d" grip--port)
                              (format " *go-grip-%d*" grip--port)
                              "go-grip"
                              (format "--port=%d" grip--port)
                              (format "--theme=%s" grip-theme)
                              "--browser=false"
                              "--bounding-box=false"
                              (format "%s.md" (file-name-base grip--preview-file))))

         (when (process-live-p grip--process)
           (grip--browse-url (grip--preview-url))
           (message "%s: Preview `%s' on %s"
                    grip--command
                    (abbreviate-file-name buffer-file-name)
                    (grip--preview-url)))))
      ('grip
       (unless (executable-find "grip")
         (grip-mode -1)
         (user-error "The `grip' is not available in PATH environment"))

       (when grip--preview-file
         (setq grip--process
               (start-process (format "grip-%d" grip--port)
                              (format " *grip-%d*" grip--port)
                              "grip"
                              (format "--api-url=%s" grip-github-api-url)
                              (format "--user=%s" grip-github-user)
                              (format "--pass=%s" grip-github-password)
                              (format "--title=%s - Grip" (file-name-nondirectory grip--preview-file))
                              grip--preview-file
                              (number-to-string grip--port)))

         (sleep-for grip-sleep-time)

         (when (process-live-p grip--process)
           (grip--browse-url (grip--preview-url))
           (message "%s: Preview `%s' on %s"
                    grip--command
                    (abbreviate-file-name buffer-file-name)
                    (grip--preview-url)))))
      (_
       (grip-mode -1)
       (user-error "No grip command is available in PATH environment")))))

(defun grip--kill-process ()
  "Kill the preview process."
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

(defun grip--preview-md ()
  "Render and preview markdown with grip."
  (setq grip--preview-file buffer-file-name)
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
  (remove-hook 'after-save-hook #'grip-org-to-md t)
  (remove-hook 'after-revert-hook #'grip-org-to-md t)
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
  (if (process-live-p grip--process)
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
