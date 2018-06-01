(require 'seeing-is-believing)

(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-hook 'enh-ruby-mode-hook 'seeing-is-believing)
(add-hook 'enh-ruby-mode-hook 'projectile-rails-on)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'safe-linum-mode)
(add-hook 'enh-ruby-mode-hook 'smartscan-mode)
(add-hook 'enh-ruby-mode-hook 'rspec-enable-appropriate-mode)

(setq enh-ruby-bounce-deep-indent nil)
(setq enh-ruby-hanging-brace-indent-level 2)
(setq ruby-deep-indent-paren nil)
(setq enh-ruby-hanging-paren-indent-level 2)
(setq rspec-use-rake-when-possible nil)
(setq enh-ruby-add-encoding-comment-on-save nil)

(remove-hook 'ruby-mode-hook 'ruby-end-mode)
(remove-hook 'enh-ruby-mode-hook 'ruby-end-mode)
