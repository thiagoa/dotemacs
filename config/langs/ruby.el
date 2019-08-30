;;; ruby.el --- Ruby config
;;
;;; Commentary:

;;; Code:

(require 'rubocop)
(require 'rspec-mode)
(require 'projectile-rails)
(require 'config-base)

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

(mapc (lambda (mode)
        (add-hook mode (lambda ()
                         (projectile-rails-on)
                         (safe-linum-mode)
                         (smartscan-mode)
                         (rubocop-mode))))
      '(ruby-mode enh-ruby-mode-hook))
(add-hook 'rspec-after-verification-hook 'finish-test-compilation)
(add-hook 'web-mode-hook 'projectile-rails-on)
(add-hook 'inf-ruby-mode-hook (lambda () (turn-on-comint-history ".pry_history")))

(eval-after-load 'rspec-mode
  (yas-load-directory (concat emacs-d "snippets/rspec-mode")))

(remove-hook 'enh-ruby-mode-hook 'ruby-end-mode)
(remove-hook 'ruby-mode-hook 'ruby-end-mode)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(add-hook
 'rbtagger-after-generate-tag-hook
 (lambda (success project-name)
   (if success
       (notify-os (concat project-name " tags generated successfully 👍") "Hero")
     (notify-os "Is this a Ruby project? Is bundler able to run? Tags generation FAILED! 👎" "Basso"))))

(setq enh-ruby-hanging-brace-deep-indent-level 1)
(setq ruby-align-chained-calls t)
(setq projectile-rails-expand-snippet nil)
(setq ruby-deep-indent-paren nil)
(setq rspec-use-rake-when-possible nil)
(setq rubocop-check-command "rubocop --format emacs -D")

(provide 'ruby)
;;; ruby.el ends here
