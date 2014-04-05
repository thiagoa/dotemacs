;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETUP ENHANCED RUBY MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-to-list 'ac-modes 'enh-ruby-mode)
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;;;;;;;;;;;
;; IDENT ;;
;;;;;;;;;;;

(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)
(setq ruby-deep-indent-paren nil)

;;;;;;;;;
;; RVM ;;
;;;;;;;;;

(require 'rvm)
(rvm-use-default)
(add-hook 'ruby-mode-hook (lambda () (rvm-activate-corresponding-ruby)))

;;;;;;;;;;;
;; RSPEC ;;
;;;;;;;;;;;

(require 'rspec-mode)
(setq rspec-use-rake-when-possible nil)

;; Use bash for RSpec (ZSH compatibility issues)
(defadvice rspec-compile (around rspec-compile-around)
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

;;;;;;;;;;;;;
;; PLUGINS ;;
;;;;;;;;;;;;;

(require 'ruby-tools)
;(require 'rcodetools)
(require 'rhtml-mode)
(require 'haml-mode)

(add-hook 'projectile-mode-hook 'projectile-rails-on)
(add-hook 'ehn-ruby-mode-hook 'projectile-rails-on)
(add-hook 'ruby-mode-hook 'projectile-rails-on)
(add-hook 'ehn-ruby-mode-hook 'robe-mode)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

;; Disable auto insert of encoding comment (was useful for 1.9)
(setq ruby-insert-encoding-magic-comment nil)

;;;;;;;;;;;;;;
;; MAPPINGS ;;
;;;;;;;;;;;;;;

;; Fire IRB session
(global-set-key (kbd "C-c r r") 'inf-ruby)

;; Live ruby results with xmpfilter (from rcodetools)
(define-key ruby-mode-map (kbd "C-c C-c") 'xmp)

;; Magic easy string interpolation
(define-key ruby-mode-map (kbd "#") 'senny-ruby-interpolate)
