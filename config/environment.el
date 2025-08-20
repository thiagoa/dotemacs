;;; environment.el --- Environment variables
;;
;;; Commentary:

;;; Code:

(require 'projectile)

(setenv "LESS_OPEN" nil)
(setenv "VISUAL" "emacsclient")
(setenv "EDITOR" (getenv "VISUAL"))
(setenv "PAGER" "cat")
(setenv "TERM" "xterm-256-color")
(setenv "SKIP_COVERAGE" "1")

(defun load-env-file (file)
  "Load environment variables from FILE into Emacs' environment."
  (setq file (concat (projectile-project-root) ".env.development"))
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (dolist (line (split-string (buffer-string) "\n" t))
        (unless (or (string-prefix-p "#" line) ; skip comments
                    (string-blank-p line))    ; skip blanks
          (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" line)
            (let ((key (match-string 1 line))
                  (value (match-string 2 line)))
              (setenv key value))))))))

;;; environment.el ends here
