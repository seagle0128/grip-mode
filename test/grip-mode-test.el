;;; grip-mode-test.el --- Tests for grip-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'tramp)
(require 'grip-mode)

(ert-deftest grip-remote-markdown-uses-local-preview-copy ()
  (let ((grip-real-time-refresh nil))
    (with-temp-buffer
      (insert "initial preview\n")
      (setq buffer-file-name "/ssh:example:/tmp/remote.md"
            default-directory "/ssh:example:/tmp/")
      (let (process-directory preview-file)
        (cl-letf (((symbol-function 'grip-start-process)
                   (lambda () (setq process-directory default-directory))))
          (unwind-protect
              (progn
                (grip--preview-md)
                (setq preview-file grip--preview-file)
                (should (not (file-remote-p preview-file)))
                (should (file-exists-p preview-file))
                (should (equal process-directory
                               (file-name-directory preview-file)))
                (with-temp-buffer
                  (insert-file-contents preview-file)
                  (should (equal (buffer-string) "initial preview\n")))
                ;; Saving the remote buffer updates the local file watched by
                ;; go-grip/mdopen.
                (erase-buffer)
                (insert "saved preview\n")
                (run-hooks 'after-save-hook)
                (with-temp-buffer
                  (insert-file-contents preview-file)
                  (should (equal (buffer-string) "saved preview\n"))))
            (grip-stop-preview)))))))

(ert-deftest grip-local-markdown-keeps-using-original-file ()
  (let ((grip-real-time-refresh nil)
        (file (make-temp-file "grip-local-" nil ".md")))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name file
                default-directory (file-name-directory file))
          (let (process-directory)
            (cl-letf (((symbol-function 'grip-start-process)
                       (lambda () (setq process-directory default-directory))))
              (grip--preview-md)
              (should (equal grip--preview-file file))
              (should (equal process-directory default-directory))))
      (delete-file file)))))

(provide 'grip-mode-test)
;;; grip-mode-test.el ends here
