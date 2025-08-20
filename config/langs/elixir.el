;;; elixir.el --- Elixir config
;;
;;; Commentary:

;;; Code:

(require 'company)

(elixir-set-source-dir)

(add-hook 'alchemist-iex-mode-hook 'company-mode)
(add-hook 'elixir-mode-hook 'safe-linum-mode)
(add-hook 'elixir-mode-hook 'alchemist-mode)
(add-hook 'elixir-mode-hook 'company-mode)
(add-hook 'elixir-mode-hook 'smartscan-mode)
(add-hook 'elixir-mode-hook (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
(add-to-list 'elixir-mode-hook 'auto-activate-ruby-end-mode-for-elixir-mode)
;; Place Elixir projects first to avoid them being confused with Yarn
;; projects. Elixir projects often have Yarn artifacts.
(let* ((found (cl-find-if (lambda (v) (eq (car v) 'elixir)) projectile-project-types)))
  (when found
    (setq projectile-project-types
          (cons found (remove found projectile-project-types)))))

;;; elixir.el ends here
