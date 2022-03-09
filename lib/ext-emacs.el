;; Reverted this function to how it was on previous versions because
;; it is undoing the transient mark when triggering any movement after
;; exchange-point-and-mark
;;
;; TODO: Figure out what happened
(when (>= emacs-major-version 29)
  (defun exchange-point-and-mark (&optional arg)
    "Put the mark where point is now, and point where the mark is now.
This command works even when the mark is not active,
and it reactivates the mark.

If Transient Mark mode is on, a prefix ARG deactivates the mark
if it is active, and otherwise avoids reactivating it.  If
Transient Mark mode is off, a prefix ARG enables Transient Mark
mode temporarily."
    (interactive "P")
    (let ((omark (mark t))
          (temp-highlight (eq (car-safe transient-mark-mode) 'only)))
      (if (null omark)
          (user-error "No mark set in this buffer"))
      (set-mark (point))
      (goto-char omark)
      (cond (temp-highlight
             (setq-local transient-mark-mode (cons 'only transient-mark-mode)))
            ((xor arg (not (region-active-p)))
             (deactivate-mark))
            (t (activate-mark)))
      nil)))

(define-advice run-mode-hooks (:around (orig-fun &rest hooks) handle-error)
  "Ensure errors during mode hooks are more apparent."
  (condition-case err
      (apply orig-fun hooks)
    (error
     (setq mode-name
           `(:propertize ,mode-name face error
                         help-echo ,(format "%s during run-mode-hooks"
                                            (error-message-string err))))
     (display-buffer (get-buffer "*Messages*"))
     (funcall #'signal (car err) (cdr err)))))

(provide 'ext-emacs)
