;;; ruby.el --- Ruby config
;;
;;; Commentary:

;;; Code:

(require 'rubocop)
(require 'rspec-mode)
(require 'projectile-rails)
(require 'config-base)

(defvar snippet-quote-char "\"")
(defvar snippet-frozen-string-literal nil)

(defun snippet-frozen-string-literal ()
  (if snippet-frozen-string-literal
      "# frozen_string_literal: true\n\n"))

(define-key projectile-rails-mode-map (kbd "C-c b") 'projectile-rails-command-map)

(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

(eval-after-load 'rspec-mode
  (yas-load-directory (concat emacs-d "snippets/rspec-mode")))

(setq inf-ruby-eval-binding
      (concat "(defined?(IRB) && IRB.conf[:MAIN_CONTEXT] && IRB.conf[:MAIN_CONTEXT].workspace.binding) || "
              "(defined?(Pry) && binding)"))

(mapc (lambda (mode)
        (add-hook mode (lambda ()
                         (projectile-rails-on)
                         (safe-linum-mode)
                         (smartscan-mode)
                         (rubocop-mode)
                         (rbtagger-mode)
                         (simple-autopair-mode))))
      '(ruby-mode enh-ruby-mode-hook))

(add-hook 'after-save-hook
          (lambda ()
            (if (eq major-mode 'enh-ruby-mode)
                (call-interactively 'rbtagger-generate-tags))))

(add-hook 'rspec-after-verification-hook
          (lambda ()
            (setq inf-ruby-buffers
                  (delete (get-buffer "*rspec-compilation*") inf-ruby-buffers))
            (ruby-finish-test-compilation)))

(add-hook 'rspec-before-verification-hook
          (lambda ()
            (setq inf-ruby-buffers (seq-filter 'buffer-live-p inf-ruby-buffers))
            (setq inf-ruby-buffers
                  (cl-delete-if (lambda (b) (string-prefix-p "*rspec-" (buffer-name b)))
                                inf-ruby-buffers))
            (add-to-list 'inf-ruby-buffers (get-buffer "*rspec-compilation*"))))

(add-hook 'web-mode-hook 'projectile-rails-on)
(add-hook 'inf-ruby-mode-hook (lambda () (turn-on-comint-history ".pry_history")))
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook
 'rbtagger-after-generate-tag-hook
 (lambda (success project-name)
   (unless success
     (notify-os "Tags Fail"
                (concat "Is this a Ruby project? Is bundler able to run? Tags generation FAILED! 👎 Please check "
                        (format rbtagger-stderr-buffer project-name))
                "Basso"))))

(setq enh-ruby-hanging-brace-deep-indent-level 1)
(setq ruby-align-chained-calls t)
(setq projectile-rails-expand-snippet nil)
(setq ruby-deep-indent-paren nil)
(setq rspec-use-rake-when-possible nil)
(setq rake-completion-system 'helm)
(setq rspec-use-opts-file-when-available nil)
(setq rubocop-check-command "rubocop --format emacs -D")

;;; ruby.el ends here
