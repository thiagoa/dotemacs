(require 'company)

(add-hook 'elixir-mode-hook 'safe-linum-mode)
(add-hook 'elixir-mode-hook 'alchemist-mode)
(add-hook 'elixir-mode-hook 'company-mode)
(add-hook 'elixir-mode-hook 'smartscan-mode)
(add-hook 'alchemist-iex-mode-hook 'company-mode)

(eval-after-load 'elixir-mode
  '(define-key elixir-mode-map (kbd "C-x C-/") 'company-complete))

(eval-after-load 'alchemist-iex-mode
  '(define-key alchemist-iex-mode-map (kbd "C-x C-/") 'company-complete))

(add-hook 'alchemist-iex-mode-hook
	  (lambda () (local-set-key (kbd "C-x C-/") #'company-complete)))

(add-to-list 'elixir-mode-hook
	     (defun auto-activate-ruby-end-mode-for-elixir-mode ()
	       (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
		    "\\(?:^\\|\\s-+\\)\\(?:do\\)")
	       (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
	       (ruby-end-mode +1)))

(defun alchemist-run-line-and-compile ()
  (interactive)
  (alchemist-compile-this-buffer)
  (alchemist-eval-print-current-line))

(eval-after-load 'elixir-mode
  '(define-key elixir-mode-map (kbd "C-x C-e") 'alchemist-run-line-and-compile))

(defun elixir-set-source-dir ()
  (interactive)
  (let* ((path
	  (replace-regexp-in-string
	   "\n$"
	   ""
	   (shell-command-to-string "echo `asdf where elixir``asdf current elixir | cut -f1 -d' '`")))
	 (binpath (concat path "/bin")))
    (setq alchemist-goto-elixir-source-dir path)
    (setq elixir-format-elixir-path (concat binpath "/elixir"))
    (setq elixir-format-mix-path (concat binpath "/mix"))))

(elixir-set-source-dir)

(add-hook 'elixir-mode-hook
	  (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
