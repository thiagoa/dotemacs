(require 'seeing-is-believing)

(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(add-hook 'ehn-ruby-mode-hook 'seeing-is-believing)
(add-hook 'enh-ruby-mode-hook 'projectile-rails-on)
(add-hook 'enh-ruby-mode-hook 'rspec-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook (lambda () (ignore-errors(linum-mode 1))))
(add-hook 'enh-ruby-mode-hook 'smartscan-mode)

(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)
(setq ruby-deep-indent-paren nil)
(setq rspec-use-rake-when-possible nil)

(defadvice rspec-compile (around rspec-compile-around)
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(add-hook 'enh-ruby-mode-hook
	  (lambda ()
	    (set (make-local-variable imenu-generic-expression)
		 '(("Methods"  "^\\( *\\(def\\) +.+\\)" 1) ))))
