;;; grip-mode.el --- Instant GitHub-flavored Markdown/Org preview using grip.        -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline
;; Version: 2.0.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience, markdown, preview

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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
  :group 'markdown)

(defcustom grip-binary-path "grip"
  "Path to the grip binary."
  :type 'string
  :group 'grip)



(defvar-local grip-process nil
  "Handle to the inferior grip process.")

(defvar-local grip-port 1088
  "Port to the grip port.")

(defvar-local grip-preview-file nil
  "The preview file for grip process.")

(defun grip-start-process ()
  "Render and preview with grip."
  (unless grip-process
    (unless (executable-find grip-binary-path)
      (user-error "You need to have `grip' installed in PATH environment"))

    ;; Generat random port
    (while (< grip-port 3000)
      (setq grip-port (random 65535)))

    ;; Start a new grip process
    (when grip-preview-file
      (setq grip-process
            (start-process (format "grip-%d" grip-port)
                           (format " *grip-%d*" grip-port)
                           grip-binary-path
                           "--browser"
                           (format "--title=%s - Grip" (buffer-name))
                           grip-preview-file
                           (number-to-string grip-port)))))
  (message (format "Preview %s on http://localhost:%d" buffer-file-name grip-port)))

(defun grip-kill-process ()
  "Kill the grip process."
  (when grip-process
    (delete-process grip-process)
    (message "Process `%s' killed" grip-process)
    (setq grip-process nil)
    (setq grip-port 1088)

    ;; Delete temp file
    (when (and grip-preview-file
               (not (string-equal grip-preview-file buffer-file-name)))
      (delete-file grip-preview-file))))

(defun grip-preview-md ()
  "Render and preview markdown with grip."
  (setq grip-preview-file (concat buffer-file-name ".tmp"))
  (grip-refresh)
  (grip-start-process))

(declare-function org-md-export-to-markdown 'ox-md)
(defun grip-org-to-md (&rest _)
  "Render org to markdown."
  (widen)
  (deactivate-mark)
  (org-md-export-to-markdown))

(defun grip-preview-org ()
  "Render and preview org with grip."
  (setq grip-preview-file (expand-file-name (grip-org-to-md)))
  (grip-start-process)
  (add-hook 'after-change-functions #'grip-org-to-md nil t))

(defun grip-refresh (&rest _)
  "Update the `grip-preview-file'.
The optional ARGS argument is needed as this function is added to the
`after-change-functions' hook."
  (write-region nil nil grip-preview-file))

(defun grip-start-preview ()
  "Start rendering and previewing with grip."
  (when buffer-file-name
    (if (eq major-mode 'org-mode)
        (grip-preview-org)
      (grip-preview-md))
    (add-hook 'after-change-functions #'grip-refresh nil t)
    (add-hook 'kill-buffer-hook #'grip-kill-process nil t)))

(defun grip-stop-preview ()
  "Stop rendering and previewing with grip."
  (grip-kill-process)
  (remove-hook 'after-change-functions #'grip-org-to-md t)
  (remove-hook 'after-change-functions #'grip-refresh t)
  (remove-hook 'kill-buffer-hook #'grip-kill-process t))

;;;###autoload
(define-minor-mode grip-mode
  "Live Markdown preview with grip."
  :lighter " grip"
  (if grip-mode
      (grip-start-preview)
    (grip-stop-preview)))

(provide 'grip-mode)

;;; grip-mode.el ends here
