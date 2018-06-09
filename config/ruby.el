(require 'seeing-is-believing)

(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'enh-ruby-mode-hook 'seeing-is-believing)
(add-hook 'enh-ruby-mode-hook 'projectile-rails-on)
(add-hook 'web-mode-hook 'projectile-rails-on)
(add-hook 'enh-ruby-mode-hook 'safe-linum-mode)
(add-hook 'enh-ruby-mode-hook 'smartscan-mode)
(add-hook 'enh-ruby-mode-hook 'rspec-enable-appropriate-mode)
(add-hook 'web-mode 'rspec-enable-appropriate-mode)
(add-hook 'ruby-mode-hook 'rubocop-mode)
(add-hook 'inf-ruby-mode-hook (lambda () (turn-on-comint-history ".pry_history")))

(eval-after-load 'inf-ruby
  '(define-key inf-ruby-minor-mode-map (kbd "C-c C-l") nil))

(eval-after-load 'inf-ruby
  '(define-key inf-ruby-minor-mode-map (kbd "C-x l") 'ruby-load-file))

(remove-hook 'ruby-mode-hook 'ruby-end-mode)
(remove-hook 'enh-ruby-mode-hook 'ruby-end-mode)

(setq enh-ruby-bounce-deep-indent nil)
(setq enh-ruby-hanging-brace-indent-level 2)
(setq ruby-deep-indent-paren nil)
(setq enh-ruby-hanging-paren-indent-level 2)
(setq rspec-use-rake-when-possible nil)
(setq enh-ruby-add-encoding-comment-on-save nil)
