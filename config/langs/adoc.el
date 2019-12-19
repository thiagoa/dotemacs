(add-to-list 'auto-mode-alist '("\\.asciidoc$" . adoc-mode))
(autoload 'adoc-mode "adoc-mode" nil t)
(add-hook 'adoc-mode-hook 'cider-mode)
