(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defvar my-term-shell "/usr/local/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defun my-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'my-term-use-utf8)


(defun my-term-hook ()
  (goto-address-mode))

(add-hook 'term-mode-hook 'my-term-hook)

(defun turn-on-comint-history (history-file)
  (setq comint-input-ring-file-name history-file)
  (comint-read-input-ring 'silent))

(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "HISTFILE")

(add-hook 'shell-mode-hook
          (lambda ()
            (turn-on-comint-history (getenv "HISTFILE"))))

(setq comint-input-ring-separator "\n: \\([0-9]+\\):\\([0-9]+\\);")
