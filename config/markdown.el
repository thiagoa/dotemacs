(add-hook 'markdown-mode-hook 'safe-linum-mode)

(eval-after-load 'markdown-mode
  '(define-key markdown-mode-map [C-tab] 'markdown-table-align))
