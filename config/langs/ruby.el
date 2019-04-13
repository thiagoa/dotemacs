;;; ruby.el --- My Ruby configuration
;;
;;; Commentary:
;;
;; My Ruby config

;;; Code:

(require 'rubocop)
(require 'rspec-mode)
(require 'projectile-rails)
(require 'functions)

(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

(add-hook 'ruby-mode-hook (lambda ()
                            (projectile-rails-on)
                            (safe-linum-mode)
                            (smartscan-mode)
                            (rubocop-mode)))
(add-hook 'rspec-after-verification-hook 'finish-test-compilation)
(add-hook 'web-mode-hook 'projectile-rails-on)
(add-hook 'inf-ruby-mode-hook (lambda () (turn-on-comint-history ".pry_history")))

(remove-hook 'ruby-mode-hook 'ruby-end-mode)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-to-list 'rspec-before-verification-hook 'inf-ruby-switch-from-compilation)

(setq projectile-rails-expand-snippet nil)
(setq ruby-deep-indent-paren nil)
(setq rspec-use-rake-when-possible nil)
(setq rubocop-check-command "rubocop --format emacs -D")

(provide 'ruby)
;;; ruby.el ends here
