(defun setup-tide-mode ()
  (interactive)

  ;; Let's disable this by default because it's a bit slow.
  ;; Format the buffer manually with M-x prettier-js
  ;; (prettier-js-mode)

  (lsp-mode)
  (add-node-modules-path)
  (tide-setup) ;; Tips: use tide-fix to fix stuff, including adding imports
  (flycheck-mode)
  (eldoc-mode)
  (lsp-ui-mode)
  (tide-hl-identifier-mode)
  (company-mode))

;; Consider making these vars buffer local in the future
(setq company-tooltip-align-annotations t)
(setq-default typescript-indent-level 2)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq web-mode-enable-auto-quoting nil)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-attr-value-indent-offset 2)

;; Formats the buffer before saving Not useful when using prettier,
;; because prettier will already handle that
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(add-hook 'rjsx-mode-hook 'setup-tide-mode)

;; web-mode extra config
(add-hook 'web-mode-hook (lambda ()
                           (pcase (file-name-extension buffer-file-name)
                             ("tsx" (setup-tide-mode)))))

(flycheck-add-mode 'typescript-tslint 'web-mode)

(add-hook 'web-mode-hook 'company-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook #'turn-on-smartparens-mode t)
