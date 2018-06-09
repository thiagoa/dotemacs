(add-hook 'markdown-mode-hook 'safe-linum-mode)

(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map [C-tab] 'markdown-table-align))

(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map (kbd "C-x ,") 'cider-insert-last-sexp-in-repl))
